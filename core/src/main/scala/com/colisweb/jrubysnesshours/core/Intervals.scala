package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.{TimeInterval, Schedule, TimeIntervalForDate}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Intervals {

  def intervalsBetween(schedule: Schedule)(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] = {
    val localStartDate = start.toLocalDate
    val localEndDate   = end.toLocalDate

    if (localStartDate == localEndDate) {
      intervalsInSameDay(schedule, localStartDate, TimeInterval.of(start.toLocalTime, end.toLocalTime))
    } else {
      val startDayIntervals: List[TimeIntervalForDate] = intervalsInStartDay(schedule, start)
      val endDayIntervals: List[TimeIntervalForDate]   = intervalsInEndDay(schedule, end)

      val numberOfDays = Period.between(localStartDate, localEndDate).getDays

      val dayRangeIntervals: ListBuffer[TimeIntervalForDate] =
        (1 until numberOfDays) // TODO Jules: Should we be inclusive or exclusive ? `until` is exclusive.
          .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, i) =>
            val date = localStartDate.plusDays(i.toLong)
            acc ++ allIntervalsInDay(schedule, date)
          }

      startDayIntervals ++ dayRangeIntervals ++ endDayIntervals
    }
  }

  def contains(schedule: Schedule)(date: ZonedDateTime): Boolean = {
    val time      = date.withZoneSameInstant(schedule.timeZone).toLocalTime
    val localDate = date.toLocalDate

    schedule.planning
      .get(date.getDayOfWeek)
      .exists { intervals: List[TimeInterval] =>
        intervals.exists(_.contains(time)) && schedule.exceptions
          .get(localDate)
          .forall(!_.exists(_.contains(time)))
      }
  }

  private[core] def intervalsInStartDay(schedule: Schedule, start: ZonedDateTime): List[TimeIntervalForDate] = {
    val startTime = start.toLocalTime
    val startDate = start.toLocalDate

    val exceptions = schedule.exceptions.getOrElse(startDate, Nil)
    val intervals =
      schedule.planning
        .getOrElse(start.getDayOfWeek, Nil)
        .filter(_ endsAfter startTime)
        .foldLeft(ListBuffer.empty[TimeInterval]) { (acc, interval) =>
          acc += (if (interval startsBefore startTime) TimeInterval.of(startTime, interval.end) else interval)
        }
        .toList

    cutExceptions(intervals, exceptions).map(interval => TimeIntervalForDate(date = startDate, interval = interval))
  }

  private[core] def intervalsInEndDay(schedule: Schedule, end: ZonedDateTime): List[TimeIntervalForDate] = {
    val endTime = end.toLocalTime
    val endDate = end.toLocalDate

    val exceptions = schedule.exceptions.getOrElse(endDate, Nil)
    val intervals =
      schedule.planning
        .getOrElse(end.getDayOfWeek, Nil)
        .filter(_ startsBefore endTime)
        .foldLeft(ListBuffer.empty[TimeInterval]) { (acc, interval) =>
          acc += (if (interval endsAfter endTime) TimeInterval.of(interval.start, endTime) else interval)
        }
        .toList

    cutExceptions(intervals, exceptions).map(interval => TimeIntervalForDate(date = endDate, interval = interval))
  }

  private[core] def intervalsInSameDay(
      schedule: Schedule,
      date: LocalDate,
      query: TimeInterval
  ): List[TimeIntervalForDate] = {
    val exceptions = schedule.exceptions.getOrElse(date, Nil)
    val intervals =
      schedule.planning
        .getOrElse(date.getDayOfWeek, Nil)
        .filter(_ isConnected query)
        .foldLeft(ListBuffer.empty[TimeInterval]) { (acc, interval) =>
          val newInterval =
            if (interval encloses query) query
            else if (interval startsBefore query.start) TimeInterval.of(query.start, interval.end)
            else if (interval endsAfter query.end) TimeInterval.of(interval.start, query.end)
            else interval

          acc += newInterval
        }
        .toList

    cutExceptions(intervals, exceptions).map(interval => TimeIntervalForDate(date = date, interval = interval))
  }

  private[core] def allIntervalsInDay(schedule: Schedule, date: LocalDate): List[TimeIntervalForDate] = {
    val intervals  = schedule.planning.getOrElse(date.getDayOfWeek, Nil)
    val exceptions = schedule.exceptions.getOrElse(date, Nil)

    cutExceptions(intervals, exceptions).map(interval => TimeIntervalForDate(date = date, interval = interval))
  }

  private[core] def cutExceptions(
      intervals: List[TimeInterval],
      exceptions: List[TimeInterval]
  ): List[TimeInterval] = {
    @tailrec
    def rec(
        allIntervals: List[TimeInterval],
        remainingExceptions: List[TimeInterval]
    ): List[TimeInterval] =
      remainingExceptions match {
        case Nil               => allIntervals
        case exception :: rest => rec(allIntervals.flatMap(_ minus exception), rest) // TODO: can be improved: exceptions are sorted, no need to apply on intervals before
      }

    intervals match {
      case Nil      => Nil
      case nonEmpty => rec(nonEmpty, exceptions)
    }
  }

}
