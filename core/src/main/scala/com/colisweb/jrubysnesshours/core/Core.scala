package com.colisweb.jrubysnesshours.core

import java.time.{
  DateTimeException,
  DayOfWeek,
  LocalDate,
  LocalDateTime,
  LocalTime,
  Period,
  ZoneId,
  ZoneOffset,
  ZonedDateTime,
  Duration => JDuration
}
import java.util.concurrent.TimeUnit

import org.threeten.extra.Interval

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

// TODO: next business hour

object TimeInterval {
  private[core] final val utc: ZoneOffset         = ZoneOffset.UTC
  private[core] final val `1970-01-01`: LocalDate = LocalDate.of(1970, 1, 1)
  private[core] final val END_OF_DAY: LocalTime   = LocalTime.of(23, 59, 0)
}

final case class TimeInterval(start: LocalTime, end: LocalTime) {
  assert(start isBefore end)

  import TimeInterval._

  @inline private[this] def toInstant(localTime: LocalTime) = LocalDateTime.of(`1970-01-01`, localTime).toInstant(utc)
  private lazy val _interval                                = Interval.of(toInstant(start), toInstant(end))

  def isBefore(that: TimeInterval): Boolean    = this._interval isBefore that._interval
  def encloses(that: TimeInterval): Boolean    = this._interval encloses that._interval
  def isConnected(that: TimeInterval): Boolean = this._interval isConnected that._interval

  @inline def overlapOnlyOnTheStart(that: TimeInterval): Boolean =
    (this startsBefore that.start) && (this endsBefore that.end)

  /**
    * Non commutative substraction: x - y != y - x
    *
    * The passed interval will be substracted from the current interval.
    */
  def minus(that: TimeInterval): List[TimeInterval] =
    if (that._interval encloses this._interval) List.empty
    else if (this._interval encloses that._interval) {
      val acc = ListBuffer.empty[TimeInterval]
      if (this.start != that.start) acc += TimeInterval(start = this.start, end = that.start)
      if (this.end != that.end) acc += TimeInterval(start = that.end, end = this.end)
      acc.toList
    } else if (!(this._interval isConnected that._interval)) this :: Nil
    else if (this overlapOnlyOnTheStart that) TimeInterval(start = this.start, end = that.start) :: Nil
    else TimeInterval(that.end, this.end) :: Nil // overlapOnlyOnTheEnd

  def contains(time: LocalTime): Boolean             = this._interval.contains(toInstant(time))
  @inline def endsBefore(time: LocalTime): Boolean   = this.end isBefore time
  @inline def endsAfter(time: LocalTime): Boolean    = this.end isAfter time
  @inline def startsBefore(time: LocalTime): Boolean = this.start isBefore time
  @inline def startsAfter(time: LocalTime): Boolean  = this.start isAfter time

  /**
    * Copied from `org.threeten.extra.Interval`.
    *
    * Here, it's not possible to directly use `this._interval union that._interval` because we're unable to
    * then convert the result of that call to a `TimeInterval` because we can't easily convert an `Instant` to
    * a `LocalTime`.
    */
  def union(that: TimeInterval): TimeInterval = {
    if (!isConnected(that)) throw new DateTimeException(s"Intervals do not connect: $this and $that")

    val cmpStart = start.compareTo(that.start)
    val cmpEnd   = end.compareTo(that.end)

    if (cmpStart >= 0 && cmpEnd <= 0) that
    else if (cmpStart <= 0 && cmpEnd >= 0) this
    else {
      val newStart = if (cmpStart >= 0) that.start else start
      val newEnd   = if (cmpEnd <= 0) that.end else end
      TimeInterval(start = newStart, end = newEnd)
    }
  }
}

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime)

final case class TimeIntervalForWeekDay(dayOfWeek: DayOfWeek, interval: TimeInterval)

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  @inline def start: LocalTime = interval.start
  @inline def end: LocalTime   = interval.end
  lazy val duration: Duration  = Duration(JDuration.between(start, end).toSeconds, TimeUnit.SECONDS)
}

final case class Schedule private[core] (
    planning: Map[DayOfWeek, List[TimeInterval]],
    exceptions: Map[LocalDate, List[TimeInterval]],
    timeZone: ZoneId
) {
  @inline def planningFor(dayOfWeek: DayOfWeek): List[TimeInterval] = planning.getOrElse(dayOfWeek, Nil)
  @inline def exceptionFor(date: LocalDate): List[TimeInterval]     = exceptions.getOrElse(date, Nil)

  def intervalsBetween(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] = {
    val localStartDate = start.toLocalDate
    val localEndDate   = end.toLocalDate

    if (localStartDate == localEndDate) {
      intervalsInSameDay(localStartDate, TimeInterval(start.toLocalTime, end.toLocalTime))
    } else {
      val startDayIntervals: List[TimeIntervalForDate] = intervalsInStartDay(start)
      val endDayIntervals: List[TimeIntervalForDate]   = intervalsInEndDay(end)

      val numberOfDays = Period.between(localStartDate, localEndDate).getDays

      val dayRangeIntervals: ListBuffer[TimeIntervalForDate] =
        (1 until numberOfDays)
          .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, i) =>
            val date = localStartDate.plusDays(i.toLong)
            acc ++ allIntervalsInDay(date)
          }

      startDayIntervals ++ dayRangeIntervals ++ endDayIntervals
    }
  }

  // TODO: To Test
  def contains(date: ZonedDateTime): Boolean = {
    val time      = date.withZoneSameInstant(timeZone).toLocalTime
    val localDate = date.toLocalDate

    @inline def existsPlanning =
      planningFor(date.getDayOfWeek).exists(_.contains(time))

    @inline def notExistsException =
      !exceptionFor(localDate).exists(_.contains(time))

    existsPlanning && notExistsException
  }

  private[core] def intervalsInStartDay(start: ZonedDateTime): List[TimeIntervalForDate] =
    allIntervalsInDay(start.toLocalDate, List(TimeInterval(start = LocalTime.MIDNIGHT, start.toLocalTime)))

  private[core] def intervalsInEndDay(end: ZonedDateTime): List[TimeIntervalForDate] =
    allIntervalsInDay(end.toLocalDate, List(TimeInterval(start = end.toLocalTime, TimeInterval.END_OF_DAY)))

  private[core] def intervalsInSameDay(
      date: LocalDate,
      query: TimeInterval
  ): List[TimeIntervalForDate] =
    allIntervalsInDay(
      date,
      List(
        TimeInterval(start = LocalTime.MIDNIGHT, query.start),
        TimeInterval(start = query.end, TimeInterval.END_OF_DAY)
      )
    )

  def allIntervalsInDay(date: LocalDate, exception: List[TimeInterval] = Nil): List[TimeIntervalForDate] = {
    val exceptions = exceptionFor(date)
    val intervals  = planningFor(date.getDayOfWeek)

    Schedule
      .cutExceptions(intervals, exception ::: exceptions)
      .map(interval => TimeIntervalForDate(date = date, interval = interval))
  }

  def within(start: ZonedDateTime, end: ZonedDateTime): Duration =
    intervalsBetween(start, end).map(_.duration).reduce(_ plus _)

  def isOpenForDurationInDate(date: LocalDate, duration: Duration): Boolean = {
    val start = ZonedDateTime.of(date, LocalTime.MIN, timeZone)
    val end   = ZonedDateTime.of(date, TimeInterval.END_OF_DAY, timeZone)

    intervalsBetween(start, end).exists(_.duration >= duration)
  }

  def isOpen(instant: ZonedDateTime): Boolean = intervalsBetween(instant, instant).nonEmpty
}

object Schedule {

  def apply(
      plannings: List[TimeIntervalForWeekDay],
      exceptions: List[DateTimeInterval],
      timeZone: ZoneId
  ): Schedule = {
    def mergeIntervals(invervals: List[TimeInterval]): List[TimeInterval] = {
      def mergeTwoIntervals(interval1: TimeInterval, interval2: TimeInterval): List[TimeInterval] =
        if (interval1 isBefore interval2) List(interval1, interval2)
        else if (interval1 encloses interval2) List(interval1)
        else List(interval1.union(interval2))

      invervals
        .sortBy(_.start)
        .foldRight(List.empty[TimeInterval]) {
          case (interval, h :: t) => mergeTwoIntervals(interval, h) ::: t // TODO: `:::` is not in constant time.
          case (interval, Nil)    => List(interval)
        }
    }

    def dateTimeIntervalsToExceptions: Map[LocalDate, List[TimeInterval]] = {
      exceptions
        .flatMap { dateTimeInterval: DateTimeInterval =>
          val numberOfDays =
            Period.between(dateTimeInterval.start.toLocalDate, dateTimeInterval.end.toLocalDate).getDays

          val localStartTime = dateTimeInterval.start.toLocalTime
          val localEndTime   = dateTimeInterval.end.toLocalTime

          val localStartDate = dateTimeInterval.start.toLocalDate

          if (numberOfDays == 0) {
            val newInterval = TimeInterval(start = localStartTime, end = localEndTime)
            List(TimeIntervalForDate(date = localStartDate, interval = newInterval))
          } else {
            val midDays =
              (1 until numberOfDays)
                .map { i =>
                  val date        = localStartDate.plusDays(i.toLong)
                  val newInterval = TimeInterval(start = LocalTime.MIDNIGHT, end = TimeInterval.END_OF_DAY)
                  TimeIntervalForDate(date = date, interval = newInterval)
                }

            val firstDay =
              TimeIntervalForDate(
                date = localStartDate,
                interval = TimeInterval(start = localStartTime, end = TimeInterval.END_OF_DAY)
              )

            val lastDay =
              TimeIntervalForDate(
                date = dateTimeInterval.end.toLocalDate,
                interval = TimeInterval(start = LocalTime.MIDNIGHT, end = localEndTime)
              )

            firstDay +: midDays :+ lastDay
          }
        }
        .groupBy(_.date)
        .mapValues(intervals => mergeIntervals(intervals.map(_.interval)))
    }

    Schedule(
      planning = plannings.groupBy(_.dayOfWeek).mapValues(intervals => mergeIntervals(intervals.map(_.interval))),
      exceptions = dateTimeIntervalsToExceptions,
      timeZone = timeZone
    )
  }

  private[core] def cutExceptions(intervals: List[TimeInterval], exceptions: List[TimeInterval]): List[TimeInterval] =
    intervals.flatMap { interval =>
      exceptions.foldLeft(List(interval)) {
        case (acc, exception) => acc.flatMap(_ minus exception)
      }
    }
}
