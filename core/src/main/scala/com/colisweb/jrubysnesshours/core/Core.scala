package com.colisweb.jrubysnesshours.core

import java.time._
import scala.math.Ordering.Implicits._

object Core {

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

      Schedule(
        plannings.groupBy(_.dayOfWeek).map {
          case (dayOfWeek, intervals) => dayOfWeek -> prepareWeekDayIntervals(intervals)
        },
        dateTimeIntervalsToExceptions(exceptions),
        timeZone
      )
    }

    private def mergeTwoIntervals(interval1: TimeInterval, interval2: TimeInterval): List[TimeInterval] = {

      if (interval2.start > interval1.end) {
        List(interval1, interval2)
      } else if (interval2.end < interval1.end) {
        List(interval1)
      } else {
        List(TimeInterval(interval1.start, interval2.end))
      }
    }

    private def prepareWeekDayIntervals(intervals: List[TimeIntervalForWeekDay]): List[TimeInterval] =
      intervals
        .sortBy(_.interval.start)
        .foldRight(List.empty[TimeInterval]) {
          case (dayInterval, h :: t) =>
            mergeTwoIntervals(dayInterval.interval, h) ::: t
          case (dayInterval, Nil) => List(dayInterval.interval)
        }

    private[core] def dateTimeIntervalsToExceptions(
        dateTimeIntervals: List[DateTimeInterval]
    ): Map[LocalDate, List[TimeInterval]] = {

      def prepareTimeIntervalForDates(intervals: List[TimeIntervalForDate]): List[TimeInterval] =
        intervals
          .sortBy(_.interval.start)
          .foldRight(List.empty[TimeInterval]) {
            case (dateInterval, h :: t) =>
              mergeTwoIntervals(dateInterval.interval, h) ::: t
            case (dateInterval, Nil) => List(dateInterval.interval)
          }

      dateTimeIntervals
        .flatMap { dateTimeInterval =>
          val numberOfDays =
            Period.between(dateTimeInterval.start.toLocalDate, dateTimeInterval.end.toLocalDate).getDays

          if (numberOfDays == 0) {
            List(
              TimeIntervalForDate(
                date = dateTimeInterval.start.toLocalDate,
                TimeInterval(start = dateTimeInterval.start.toLocalTime, end = dateTimeInterval.end.toLocalTime)
              )
            )
          } else {
            val dayRangeIntervals = Range(1, numberOfDays)

            val midDays =
              dayRangeIntervals.map { i =>
                val dateTime = dateTimeInterval.start.plusDays(i.toLong)
                val date     = dateTime.toLocalDate

                val newInterval = TimeInterval(start = LocalTime.MIDNIGHT, end = LocalTime.of(23, 59))

                TimeIntervalForDate(date = date, interval = newInterval)
              }

            val firstDay = TimeIntervalForDate(
              date = dateTimeInterval.start.toLocalDate,
              TimeInterval(start = dateTimeInterval.start.toLocalTime, end = LocalTime.of(23, 59))
            )
            val lastDay = TimeIntervalForDate(
              date = dateTimeInterval.end.toLocalDate,
              TimeInterval(start = LocalTime.MIDNIGHT, end = dateTimeInterval.end.toLocalTime)
            )

            firstDay +: midDays :+ lastDay
          }
        }
        .groupBy(_.date)
        .mapValues(prepareTimeIntervalForDates)
    }
  }

  def within(schedule: Schedule)(start: ZonedDateTime, end: ZonedDateTime): Duration = {

    Intervals
      .intervalsBetween(schedule)(start, end)
      .foldLeft(Duration.ZERO)(
        (total, segment) => total.plus(Duration.between(segment.startTime, segment.endTime))
      )
  }

  def isOpenForDurationInDate(schedule: Schedule)(date: LocalDate, duration: Duration): Boolean = {

    val start = ZonedDateTime.of(date, LocalTime.MIN, schedule.timeZone)
    val end   = ZonedDateTime.of(date, LocalTime.MAX, schedule.timeZone)

    Intervals
      .intervalsBetween(schedule)(start, end)
      .exists(
        segment => Duration.between(segment.startTime, segment.endTime) >= duration
      )
  }

  def isOpen(schedule: Schedule)(instant: ZonedDateTime): Boolean = {

    Intervals
      .intervalsBetween(schedule)(instant, instant)
      .nonEmpty
  }

  // TODO: next business hour
}
