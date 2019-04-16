package com.colisweb.jrubysnesshours.core

import java.time._
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration

final case class Schedule private[core] (
    planning: Map[DayOfWeek, List[TimeInterval]],
    exceptions: Map[LocalDate, List[TimeInterval]],
    timeZone: ZoneId
) {
  @inline def planningFor(dayOfWeek: DayOfWeek): List[TimeInterval] = planning.getOrElse(dayOfWeek, Nil)

  @inline def exceptionFor(date: LocalDate): List[TimeInterval] = exceptions.getOrElse(date, Nil)

  def intervalsBetween(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] = {
    val localStart     = start.withZoneSameInstant(timeZone).toLocalDateTime
    val localEnd       = end.withZoneSameInstant(timeZone).toLocalDateTime
    val localStartDate = localStart.toLocalDate
    val localEndDate   = localEnd.toLocalDate

    if (localStartDate == localEndDate) {
      intervalsInSameDay(localStartDate, TimeInterval(localStart.toLocalTime, localEnd.toLocalTime))
    } else {
      val startDayIntervals: List[TimeIntervalForDate] = intervalsInStartDay(localStart)
      val endDayIntervals: List[TimeIntervalForDate]   = intervalsInEndDay(localEnd)

      val numberOfDays = localStartDate.until(localEndDate, ChronoUnit.DAYS)

      val dayRangeIntervals: ListBuffer[TimeIntervalForDate] =
        (1L until numberOfDays)
          .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, i) =>
            val date = localStartDate.plusDays(i)
            acc ++ allIntervalsInDate(date)
          }

      startDayIntervals ++ dayRangeIntervals ++ endDayIntervals
    }
  }

  def within(start: ZonedDateTime, end: ZonedDateTime): Duration =
    intervalsBetween(start, end).map(_.duration).reduce(_ plus _)

  // TODO: To Test
  def contains(instant: ZonedDateTime): Boolean = {
    val localInstant = instant.withZoneSameInstant(timeZone).toLocalDateTime
    val date         = localInstant.toLocalDate
    val time         = localInstant.toLocalTime

    @inline def existsPlanning =
      planningFor(instant.getDayOfWeek).exists(_.contains(time))

    @inline def notExistsException =
      !exceptionFor(date).exists(_.contains(time))

    existsPlanning && notExistsException
  }

  // TODO: To Test
  def isOpenForDurationInDate(date: LocalDate, duration: Duration): Boolean = {
    val start = ZonedDateTime.of(date, LocalTime.MIN, timeZone)
    val end   = ZonedDateTime.of(date, LocalTime.MAX, timeZone)

    intervalsBetween(start, end).exists(_.duration >= duration)
  }

  def nextOpenTimeAfter(instant: ZonedDateTime): Option[ZonedDateTime] = {
    if (planning.nonEmpty) {

      @tailrec
      def findNextOpenTimeAfter(date: LocalDate, additionalException: List[TimeInterval]): Option[LocalDateTime] =
        allIntervalsInDate(date, additionalException) match {
          case interval :: _ => Some(LocalDateTime.of(date, interval.start))
          case Nil           => findNextOpenTimeAfter(date.plusDays(1), Nil)
        }

      val localInstant = instant.withZoneSameInstant(timeZone).toLocalDateTime
      val date         = localInstant.toLocalDate
      val time         = localInstant.toLocalTime

      findNextOpenTimeAfter(date, startOfDayCut(time))
        .map(_.atZone(instant.getZone))

    } else None
  }

  private[core] def startOfDayCut(start: LocalTime): List[TimeInterval] =
    if (start == LocalTime.MIN) Nil
    else List(TimeInterval(start = LocalTime.MIN, end = start))

  private[core] def endOfDayCut(end: LocalTime): List[TimeInterval] =
    if (end == LocalTime.MAX) Nil
    else List(TimeInterval(start = end, end = LocalTime.MAX))

  private[core] def intervalsInStartDay(start: LocalDateTime): List[TimeIntervalForDate] =
    allIntervalsInDate(start.toLocalDate, startOfDayCut(start.toLocalTime))

  private[core] def intervalsInEndDay(end: LocalDateTime): List[TimeIntervalForDate] =
    allIntervalsInDate(end.toLocalDate, endOfDayCut(end.toLocalTime))

  private[core] def intervalsInSameDay(date: LocalDate, query: TimeInterval): List[TimeIntervalForDate] =
    allIntervalsInDate(date, startOfDayCut(query.start) ++ endOfDayCut(query.end))

  private[core] def allIntervalsInDate(
      date: LocalDate,
      additionalExceptions: List[TimeInterval] = Nil
  ): List[TimeIntervalForDate] =
    Schedule
      .cutExceptions(planningFor(date.getDayOfWeek), additionalExceptions ::: exceptionFor(date))
      .map(interval => TimeIntervalForDate(date = date, interval = interval))

}
object Schedule {

  def apply(
      planning: List[TimeIntervalForWeekDay],
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
          val localStartTime = dateTimeInterval.start.toLocalTime
          val localEndTime   = dateTimeInterval.end.toLocalTime

          val localStartDate = dateTimeInterval.start.toLocalDate
          val localEndDate   = dateTimeInterval.end.toLocalDate

          val numberOfDays = localStartDate.until(localEndDate, ChronoUnit.DAYS)

          if (numberOfDays == 0L) {
            val newInterval = TimeInterval(start = localStartTime, end = localEndTime)
            List(TimeIntervalForDate(date = localStartDate, interval = newInterval))
          } else {
            val midDays =
              (1L until numberOfDays)
                .map { i =>
                  val date        = localStartDate.plusDays(i)
                  val newInterval = TimeInterval(start = LocalTime.MIN, end = LocalTime.MAX)
                  TimeIntervalForDate(date = date, interval = newInterval)
                }

            val firstDay =
              TimeIntervalForDate(
                date = localStartDate,
                interval = TimeInterval(start = localStartTime, end = LocalTime.MAX)
              )

            val lastDay =
              TimeIntervalForDate(
                date = localEndDate,
                interval = TimeInterval(start = LocalTime.MIN, end = localEndTime)
              )

            firstDay +: midDays :+ lastDay
          }
        }
        .groupBy(_.date)
        .mapValues(intervals => mergeIntervals(intervals.map(_.interval)))
    }

    Schedule(
      planning = planning.groupBy(_.dayOfWeek).mapValues(intervals => mergeIntervals(intervals.map(_.interval))),
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
