package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.Schedule

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._
import TimeInterval.TimeIntervalOps

object Segments {

  def segmentsBetween(
      schedule: Schedule
  )(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] = {

    val localStart =
      start.withZoneSameInstant(schedule.timeZone).toLocalDateTime
    val localEnd = end.withZoneSameInstant(schedule.timeZone).toLocalDateTime

    if (start.toLocalDate == end.toLocalDate) {
      segmentsInOneDay(schedule.planning, schedule.exceptions)(
        start.toLocalDate,
        TimeInterval(start.toLocalTime, end.toLocalTime)
      )
    } else {
      val startDaySegments =
        segmentsInStartDay(schedule.planning, schedule.exceptions)(localStart)
      val endDaySegments =
        segmentsInEndDay(schedule.planning, schedule.exceptions)(localEnd)

      val numberOfDays =
        Period.between(localStart.toLocalDate, localEnd.toLocalDate).getDays
      val dayRangeSegments = Range(1, numberOfDays)
        .foldLeft(Nil: List[TimeIntervalForDate]) { (allSegments, i) =>
          val date = localStart.plusDays(i.toLong)
          allSegments ++ allSegmentsInDay(
            schedule.planning,
            schedule.exceptions
          )(
            date.toLocalDate
          )
        }

      startDaySegments ++ dayRangeSegments ++ endDaySegments
    }
  }

  def contains(schedule: Schedule)(instant: ZonedDateTime): Boolean = {
    val localInstant = instant.withZoneSameInstant(schedule.timeZone)

    schedule.planning
      .get(localInstant.getDayOfWeek)
      .exists(
        interval =>
          if (interval.exists(_.contains(localInstant.toLocalTime))) {
            schedule.exceptions
              .get(localInstant.toLocalDate)
              .forall(_.forall(_.containsNot(localInstant.toLocalTime)))
          } else false
      )
  }

  private[core] def segmentsInStartDay(
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]]
  )(start: LocalDateTime): List[TimeIntervalForDate] = {

    val applyExceptionsTo =
      applyExceptionsToInterval(exceptions, start.toLocalDate)(_)

    planning
      .getOrElse(start.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeIntervalForDate]) { (result, interval) =>
        if (interval.end < start.toLocalTime)
          result
        else if (interval.start < start.toLocalTime)
          result ++ applyExceptionsTo(
            TimeInterval(start.toLocalTime, interval.end)
          )
        else
          result ++ applyExceptionsTo(interval)
      }
  }

  private[core] def segmentsInEndDay(
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]]
  )(end: LocalDateTime): List[TimeIntervalForDate] = {

    val applyExceptionsTo =
      applyExceptionsToInterval(exceptions, end.toLocalDate)(_)

    planning
      .getOrElse(end.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeIntervalForDate]) { (result, interval) =>
        if (interval.start > end.toLocalTime)
          result
        else if (interval.end > end.toLocalTime)
          result ++ applyExceptionsTo(
            TimeInterval(interval.start, end.toLocalTime)
          )
        else
          result ++ applyExceptionsTo(interval)
      }
  }

  private[core] def segmentsInOneDay(
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]]
  )(date: LocalDate, query: TimeInterval): List[TimeIntervalForDate] = {

    val applyExceptionsTo = applyExceptionsToInterval(exceptions, date)(_)

    planning
      .getOrElse(date.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeIntervalForDate]) { (result, interval) =>
        if (interval.end < query.start || interval.start > query.end)
          result
        else if (interval.start < query.start && interval.end > query.end)
          result ++ applyExceptionsTo(TimeInterval(query.start, query.end))
        else if (interval.start < query.start)
          result ++ applyExceptionsTo(TimeInterval(query.start, interval.end))
        else if (interval.end > query.end)
          result ++ applyExceptionsTo(TimeInterval(interval.start, query.end))
        else
          result ++ applyExceptionsTo(interval)
      }
  }

  private[core] def allSegmentsInDay(
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]]
  )(date: LocalDate): List[TimeIntervalForDate] = {
    planning
      .getOrElse(date.getDayOfWeek, Nil)
      .flatMap(applyExceptionsToInterval(exceptions, date))
  }

  private[core] def applyExceptionsToInterval(
      exceptions: Map[LocalDate, List[TimeInterval]],
      date: LocalDate
  )(initialInterval: TimeInterval): List[TimeIntervalForDate] = {

    @tailrec
    def applyOneByOne(
        remainingExceptions: List[TimeInterval],
        interval: TimeInterval,
        newIntervals: List[TimeIntervalForDate]
    ): List[TimeIntervalForDate] = {
      remainingExceptions match {
        case Nil => newIntervals :+ TimeIntervalForDate(date, interval)
        case exception :: remaining =>
          if (interval.start >= exception.start && interval.end <= exception.end) { // interval included in exception -> killed
            Nil
          } else if (interval.end <= exception.start || interval.start >= exception.end) { // interval outside exception -> untouched
            applyOneByOne(remaining, interval, newIntervals)
          } else if (interval.start < exception.start && interval.end <= exception.end) { // exception overlaps interval right -> shortened right
            applyOneByOne(
              remaining,
              TimeInterval(interval.start, exception.start),
              newIntervals
            )
          } else if (interval.start >= exception.start && interval.end > exception.end) { // exception overlaps interval left -> shortened left
            applyOneByOne(
              remaining,
              TimeInterval(exception.end, interval.end),
              newIntervals
            )
          } else { // () interval.start < toExclude.start && interval.end > toExclude.end //  // interval cut by exception -> cut in two
            applyOneByOne(
              remaining,
              TimeInterval(exception.end, interval.end),
              newIntervals :+ TimeIntervalForDate(
                date,
                TimeInterval(interval.start, exception.start)
              )
            )
          }
      }
    }

    applyOneByOne(exceptions.getOrElse(date, Nil), initialInterval, Nil)
  }
}
