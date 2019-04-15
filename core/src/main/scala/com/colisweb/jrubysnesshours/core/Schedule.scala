package com.colisweb.jrubysnesshours.core

import java.time._
import java.time.temporal.ChronoUnit

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration

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

      val numberOfDays = localStartDate.until(localEndDate, ChronoUnit.DAYS)

      val dayRangeIntervals: ListBuffer[TimeIntervalForDate] =
        (1L until numberOfDays)
          .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, i) =>
            val date = localStartDate.plusDays(i)
            acc ++ allIntervalsInDay(date)
          }

      startDayIntervals ++ dayRangeIntervals ++ endDayIntervals
    }
  }

  // TODO: To Test
  def contains(date: ZonedDateTime): Boolean = {
    val time      = date.withZoneSameInstant(timeZone).toLocalTime
    val localDate = date.withZoneSameInstant(timeZone).toLocalDate
    val dow       = date.getDayOfWeek

    val existsPlanning     = planningFor(dow).exists(_.contains(time))
    val notExistsException = !exceptionFor(localDate).exists(_.contains(time))

    existsPlanning && notExistsException
  }

  private[core] def intervalsInStartDay(start: ZonedDateTime): List[TimeIntervalForDate] =
    allIntervalsInDay(start.toLocalDate, List(TimeInterval(start = LocalTime.MIDNIGHT, end = start.toLocalTime)))

  private[core] def intervalsInEndDay(end: ZonedDateTime): List[TimeIntervalForDate] =
    allIntervalsInDay(end.toLocalDate, List(TimeInterval(start = end.toLocalTime, end = TimeInterval.END_OF_DAY)))

  private[core] def intervalsInSameDay(
      date: LocalDate,
      query: TimeInterval
  ): List[TimeIntervalForDate] =
    allIntervalsInDay(
      date,
      List(
        TimeInterval(start = LocalTime.MIDNIGHT, end = query.start),
        TimeInterval(start = query.end, end = TimeInterval.END_OF_DAY)
      )
    )

  def allIntervalsInDay(date: LocalDate, exception: List[TimeInterval] = Nil): List[TimeIntervalForDate] =
    Schedule
      .cutExceptions(planningFor(date.getDayOfWeek), exception ::: exceptionFor(date))
      .map(interval => TimeIntervalForDate(date = date, interval = interval))

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
                date = localEndDate,
                interval = TimeInterval(start = LocalTime.MIDNIGHT, end = localEndTime)
              )

            firstDay +: midDays :+ lastDay
          }
        }
        .groupBy(_.date)
        .mapValues(intervals => mergeIntervals(intervals.map(_.interval)))
    }

    val p = planning.groupBy(_.dayOfWeek).mapValues(intervals => mergeIntervals(intervals.map(_.interval)))

    Schedule(
      planning = p,
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
