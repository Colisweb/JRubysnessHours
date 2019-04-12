package com.colisweb.jrubysnesshours.core

import java.time._

import org.threeten.extra.Interval

import scala.collection.mutable.ListBuffer

object Core {

  import scala.math.Ordering.Implicits._

  private[core] final val utc: ZoneOffset         = ZoneOffset.UTC
  private[core] final val `1970-01-01`: LocalDate = LocalDate.of(1970, 1, 1)

  /**
    * More info on the `sealed abstract case class` pattern:
    *   - https://gist.github.com/tpolecat/a5cb0dc9adeacc93f846835ed21c92d2
    *   - https://nrinaudo.github.io/scala-best-practices/tricky_behaviours/final_case_classes.html
    */
  sealed abstract case class TimeInterval(start: LocalTime, end: LocalTime) {
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
        if (this.start != that.start) acc += TimeInterval.of(start = this.start, end = that.start)
        if (this.end != that.end) acc += TimeInterval.of(start = that.end, end = this.end)
        acc.toList
      } else if (!(this._interval isConnected that._interval)) this :: Nil
      else if (this overlapOnlyOnTheStart that) TimeInterval.of(this.start, that.start) :: Nil
      else TimeInterval.of(that.end, this.end) :: Nil // overlapOnlyOnTheEnd

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
        TimeInterval.of(start = newStart, end = newEnd)
      }
    }
  }

  object TimeInterval {
    def of(start: LocalTime, end: LocalTime): TimeInterval = {
      assert(start isBefore end) // Ugly

      new TimeInterval(start, end) {}
    }
  }

  final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime)

  final case class TimeIntervalForWeekDay(dayOfWeek: DayOfWeek, interval: TimeInterval)

  final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
    lazy val start: LocalTime   = interval.start
    lazy val end: LocalTime     = interval.end
    lazy val duration: Duration = Duration.between(start, end)
  }

  case class Schedule private[core] (
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]],
      timeZone: ZoneId
  )

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
          else if (interval1 isConnected interval2) List(interval1.union(interval2))
          else Nil // TODO Jules: Ce `Nil` me géne. Comment le virer ?

        invervals
          .sortBy(_.start)
          .foldRight(List.empty[TimeInterval]) {
            case (interval, h :: t) => mergeTwoIntervals(interval, h) ::: t
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
            // val localEndDate = dateTimeInterval.end.toLocalDate

            if (numberOfDays == 0) {
              val newInterval = TimeInterval.of(start = localStartTime, end = localEndTime)
              List(TimeIntervalForDate(date = localStartDate, interval = newInterval))
            } else {
              val midDays =
                (1 until numberOfDays)
                  .map { i =>
                    val date        = localStartDate.plusDays(i.toLong)
                    val newInterval = TimeInterval.of(start = LocalTime.MIDNIGHT, end = LocalTime.MAX) // TODO Jules: Should we really prefer MAX here ?
                    TimeIntervalForDate(date = date, interval = newInterval)
                  }

              val firstDay =
                TimeIntervalForDate(
                  date = localStartDate,
                  interval = TimeInterval.of(start = localStartTime, end = LocalTime.MAX) // TODO Jules: Should we really prefer MAX here ?
                )

              val lastDay =
                TimeIntervalForDate(
                  date = localStartDate, // TODO Jules: why `localStartDate` and not `localEndDate` ??
                  interval = TimeInterval.of(start = LocalTime.MIDNIGHT, end = localEndTime)
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
  }

  def within(schedule: Schedule)(start: ZonedDateTime, end: ZonedDateTime): Duration =
    Intervals
      .intervalsBetween(schedule)(start, end)
      .map(_.duration)
      .reduce(_ plus _)

  def isOpenForDurationInDate(schedule: Schedule)(date: LocalDate, duration: Duration): Boolean = {
    val start = ZonedDateTime.of(date, LocalTime.MIN, schedule.timeZone)
    val end   = ZonedDateTime.of(date, LocalTime.MAX, schedule.timeZone)

    Intervals
      .intervalsBetween(schedule)(start, end)
      .exists(_.duration >= duration)
  }

  def isOpen(schedule: Schedule)(instant: ZonedDateTime): Boolean =
    Intervals
      .intervalsBetween(schedule)(instant, instant)
      .nonEmpty

  // TODO: next business hour
}
