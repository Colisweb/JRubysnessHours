package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.{TimeInterval, Schedule, TimeIntervalForDate}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Intervals {

  import scala.math.Ordering.Implicits._

  def intervalsBetween(schedule: Schedule)(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] = {
    val localStartDate = start.toLocalDate
    val localEndDate   = end.toLocalDate

    if (localStartDate == localEndDate) {
      intervalsInSameDay(schedule, localStartDate, TimeInterval.of(start.toLocalTime, end.toLocalTime))
    } else {
      val startDayIntervals: ListBuffer[TimeIntervalForDate] = intervalsInStartDay(schedule, start)
      val endDayIntervals: ListBuffer[TimeIntervalForDate]   = intervalsInEndDay(schedule, end)

      val numberOfDays = Period.between(localStartDate, localEndDate).getDays

      val dayRangeIntervals: ListBuffer[TimeIntervalForDate] =
        (1 until numberOfDays) // TODO Jules: Should we be inclusive or exclusive ? `until` is exclusive.
          .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, i) =>
            val date = localStartDate.plusDays(i.toLong)
            acc ++ allIntervalsInDay(schedule, date)
          }

      (startDayIntervals ++ dayRangeIntervals ++ endDayIntervals).toList
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

  private[core] def intervalsInStartDay(schedule: Schedule, start: ZonedDateTime): ListBuffer[TimeIntervalForDate] = {
    val startTime = start.toLocalTime
    val startDate = start.toLocalDate

    schedule.planning
      .getOrElse(start.getDayOfWeek, Nil)
      .filter(_ endsAfter startTime)
      .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, interval) =>
        val newInterval: TimeInterval =
          if (interval startsBefore startTime) TimeInterval.of(startTime, interval.end) else interval

        acc ++ applyExceptionsToInterval(schedule.exceptions, startDate, newInterval)
      }
  }

  private[core] def intervalsInEndDay(schedule: Schedule, end: ZonedDateTime): ListBuffer[TimeIntervalForDate] = {
    val endTime = end.toLocalTime
    val endDate = end.toLocalDate

    schedule.planning
      .getOrElse(end.getDayOfWeek, Nil)
      .filter(_ startsBefore endTime)
      .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, interval) =>
        val newInterval =
          if (interval endsAfter endTime) TimeInterval.of(interval.start, endTime) else interval

        acc ++ applyExceptionsToInterval(schedule.exceptions, endDate, newInterval)
      }
  }

  private[core] def intervalsInSameDay(
      schedule: Schedule,
      date: LocalDate,
      query: TimeInterval
  ): List[TimeIntervalForDate] =
    schedule.planning
      .getOrElse(date.getDayOfWeek, Nil)
      .filter(_ isConnected query)
      .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, interval) =>
        val newInterval =
          if (interval encloses query) query
          else if (interval startsBefore query.start) TimeInterval.of(query.start, interval.end)
          else if (interval endsAfter query.end) TimeInterval.of(interval.start, query.end)
          else interval

        acc ++ applyExceptionsToInterval(schedule.exceptions, date, newInterval)
      }
      .toList

  private[core] def allIntervalsInDay(schedule: Schedule, date: LocalDate): List[TimeIntervalForDate] =
    schedule.planning
      .getOrElse(date.getDayOfWeek, Nil)
      .flatMap(applyExceptionsToInterval(schedule.exceptions, date, _))

  private[core] def applyExceptionsToInterval(
      exceptions: Map[LocalDate, List[TimeInterval]],
      date: LocalDate,
      initialInterval: TimeInterval
  ): List[TimeIntervalForDate] = {

    // TODO Jules: Je n'ai pas encore vraiment lu cette méthode. Les `>=` m'étonne.
    @tailrec
    def applyOneByOne(
        remainingExceptions: List[TimeInterval],
        interval: TimeInterval,
        acc: ListBuffer[TimeIntervalForDate]
    ): ListBuffer[TimeIntervalForDate] = {
      remainingExceptions match {
        case Nil => acc :+ TimeIntervalForDate(date, interval)
        case exception :: remaining =>
          if (exception encloses interval) ListBuffer.empty
          else if (interval.end <= exception.start || interval.start >= exception.end) { // interval outside exception -> untouched
            applyOneByOne(remaining, interval, acc)
          } else if (interval.start < exception.start && interval.end <= exception.end) { // exception overlaps interval right -> shortened right
            applyOneByOne(remaining, TimeInterval.of(interval.start, exception.start), acc)
          } else if (interval.start >= exception.start && interval.end > exception.end) { // exception overlaps interval left -> shortened left
            applyOneByOne(remaining, TimeInterval.of(exception.end, interval.end), acc)
          } else { // () interval.start < toExclude.start && interval.end > toExclude.end //  // interval cut by exception -> cut in two
            applyOneByOne(
              remaining,
              TimeInterval.of(exception.end, interval.end),
              acc :+ TimeIntervalForDate(date, TimeInterval.of(interval.start, exception.start))
            )
          }
      }
    }

    applyOneByOne(exceptions.getOrElse(date, Nil), initialInterval, ListBuffer.empty).toList
  }
}
