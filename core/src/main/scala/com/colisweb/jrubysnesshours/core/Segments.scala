package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval,
  TimeSegment
}

import scala.math.Ordering.Implicits._

object Segments {

  def segmentsBetween(
      planning: BusinessHoursByDayOfWeek,
      planningTimeZone: ZoneId
  )(start: ZonedDateTime, end: ZonedDateTime): List[TimeSegment] = {

    val localStart = start.withZoneSameInstant(planningTimeZone).toLocalDateTime
    val localEnd = end.withZoneSameInstant(planningTimeZone).toLocalDateTime

    if (localStart.toLocalDate == localEnd.toLocalDate) {
      segmentsInOneDay(planning)(
        localStart.toLocalDate,
        Interval(localStart.toLocalTime, localEnd.toLocalTime)
      )
    } else {
      val startDaySegments = segmentsInStartDay(planning)(localStart)
      val endDaySegments = segmentsInEndDay(planning)(localEnd)

      val numberOfDays =
        Period.between(localStart.toLocalDate, localEnd.toLocalDate).getDays
      val dayRangeSegments = Range(1, numberOfDays)
        .foldLeft(Nil: List[TimeSegment]) { (allSegments, i) =>
          val date = localStart.plusDays(i.toLong)
          allSegments ++ allSegmentsInDay(planning)(date.toLocalDate)
        }

      startDaySegments ++ dayRangeSegments ++ endDaySegments
    }
  }

  private def segmentsInStartDay(
      planning: BusinessHoursByDayOfWeek
  )(start: LocalDateTime): List[TimeSegment] = {
    planning
      .getOrElse(start.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeSegment]) { (result, interval) =>
        if (interval.endTime < start.toLocalTime)
          result
        else if (interval.startTime < start.toLocalTime)
          result :+ TimeSegment(
            start.toLocalDate,
            Interval(start.toLocalTime, interval.endTime)
          )
        else
          result :+ TimeSegment(start.toLocalDate, interval)
      }
  }

  private def segmentsInEndDay(
      planning: BusinessHoursByDayOfWeek
  )(end: LocalDateTime): List[TimeSegment] = {
    planning
      .getOrElse(end.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeSegment]) { (result, interval) =>
        if (interval.startTime > end.toLocalTime)
          result
        else if (interval.endTime > end.toLocalTime)
          result :+ TimeSegment(
            end.toLocalDate,
            Interval(interval.startTime, end.toLocalTime)
          )
        else
          result :+ TimeSegment(end.toLocalDate, interval)
      }
  }

  private def segmentsInOneDay(
      planning: BusinessHoursByDayOfWeek
  )(date: LocalDate, query: Interval): List[TimeSegment] = {
    planning
      .getOrElse(date.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeSegment]) { (result, interval) =>
        if (interval.endTime < query.startTime || interval.startTime > query.endTime)
          result
        else if (interval.startTime < query.startTime && interval.endTime > query.endTime)
          result :+ TimeSegment(date, Interval(query.startTime, query.endTime))
        else if (interval.startTime < query.startTime)
          result :+ TimeSegment(
            date,
            Interval(query.startTime, interval.endTime)
          )
        else if (interval.endTime > query.endTime)
          result :+ TimeSegment(
            date,
            Interval(interval.startTime, query.endTime)
          )
        else
          result :+ TimeSegment(date, interval)
      }
  }

  private def allSegmentsInDay(
      planning: BusinessHoursByDayOfWeek
  )(date: LocalDate): List[TimeSegment] = {
    planning.getOrElse(date.getDayOfWeek, Nil).map(TimeSegment(date, _))
  }
}
