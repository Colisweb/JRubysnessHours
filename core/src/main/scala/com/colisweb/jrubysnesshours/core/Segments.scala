package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.TimeSegment.mergeTimeSegments
import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval,
  TimeSegment
}

import scala.math.Ordering.Implicits._

object Segments {

  def segmentsBetween(
      planning: BusinessHoursByDayOfWeek,
      planningTimeZone: ZoneId,
      exceptionSegments: List[TimeSegment]
  )(start: ZonedDateTime, end: ZonedDateTime): List[TimeSegment] = {

    val localStart = start.withZoneSameInstant(planningTimeZone).toLocalDateTime
    val localEnd = end.withZoneSameInstant(planningTimeZone).toLocalDateTime
    segmentsBetween(planning, exceptionSegments)(localStart, localEnd)
  }

  def segmentsBetween(
      planning: BusinessHoursByDayOfWeek,
      exceptionSegments: List[TimeSegment]
  )(start: LocalDateTime, end: LocalDateTime): List[TimeSegment] = {

    val exceptionsByDate: Map[LocalDate, List[TimeSegment]] =
      exceptionSegments.groupBy(_.date)

    if (start.toLocalDate == end.toLocalDate) {
      segmentsInOneDay(planning, exceptionsByDate)(
        start.toLocalDate,
        Interval(start.toLocalTime, end.toLocalTime)
      )
    } else {
      val startDaySegments =
        segmentsInStartDay(planning, exceptionsByDate)(start)
      val endDaySegments = segmentsInEndDay(planning, exceptionsByDate)(end)

      val numberOfDays =
        Period.between(start.toLocalDate, end.toLocalDate).getDays
      val dayRangeSegments = Range(1, numberOfDays)
        .foldLeft(Nil: List[TimeSegment]) { (allSegments, i) =>
          val date = start.plusDays(i.toLong)
          allSegments ++ allSegmentsInDay(planning, exceptionsByDate)(
            date.toLocalDate
          )
        }

      startDaySegments ++ dayRangeSegments ++ endDaySegments
    }
  }

  private def segmentsInStartDay(
      planning: BusinessHoursByDayOfWeek,
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]]
  )(start: LocalDateTime): List[TimeSegment] = {

    val partialSplitTimeSegmentFromExceptions = splitTimeSegmentFromExceptions(
      exceptionsSegmentByDate,
      start.toLocalDate
    ) _

    planning
      .getOrElse(start.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeSegment]) { (result, interval) =>
        if (interval.endTime < start.toLocalTime)
          result
        else if (interval.startTime < start.toLocalTime)
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(
              start.toLocalDate,
              Interval(start.toLocalTime, interval.endTime)
            )
          )
        else
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(start.toLocalDate, interval)
          )
      }
  }

  private def segmentsInEndDay(
      planning: BusinessHoursByDayOfWeek,
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]]
  )(end: LocalDateTime): List[TimeSegment] = {

    val partialSplitTimeSegmentFromExceptions =
      splitTimeSegmentFromExceptions(exceptionsSegmentByDate, end.toLocalDate) _

    planning
      .getOrElse(end.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeSegment]) { (result, interval) =>
        if (interval.startTime > end.toLocalTime)
          result
        else if (interval.endTime > end.toLocalTime)
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(
              end.toLocalDate,
              Interval(interval.startTime, end.toLocalTime)
            )
          )
        else
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(end.toLocalDate, interval)
          )
      }
  }

  private def segmentsInOneDay(
      planning: BusinessHoursByDayOfWeek,
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]]
  )(date: LocalDate, query: Interval): List[TimeSegment] = {

    val partialSplitTimeSegmentFromExceptions =
      splitTimeSegmentFromExceptions(exceptionsSegmentByDate, date) _

    planning
      .getOrElse(date.getDayOfWeek, Nil)
      .foldLeft(Nil: List[TimeSegment]) { (result, interval) =>
        if (interval.endTime < query.startTime || interval.startTime > query.endTime)
          result
        else if (interval.startTime < query.startTime && interval.endTime > query.endTime)
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(date, Interval(query.startTime, query.endTime))
          )
        else if (interval.startTime < query.startTime)
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(date, Interval(query.startTime, interval.endTime))
          )
        else if (interval.endTime > query.endTime)
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(date, Interval(interval.startTime, query.endTime))
          )
        else
          result ++ partialSplitTimeSegmentFromExceptions(
            TimeSegment(date, interval)
          )
      }
  }

  private def allSegmentsInDay(
      planning: BusinessHoursByDayOfWeek,
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]]
  )(date: LocalDate): List[TimeSegment] = {
    planning.getOrElse(date.getDayOfWeek, Nil).flatMap { interval =>
      splitTimeSegmentFromExceptions(exceptionsSegmentByDate, date)(
        TimeSegment(date, interval)
      )
    }
  }

  private[core] def splitTimeSegmentFromExceptions(
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]],
      date: LocalDate
  )(initialSegment: TimeSegment) = {
    exceptionsSegmentByDate
      .getOrElse(date, Nil)
      .sortBy(_.startTime)
      .foldLeft(List(initialSegment)) { (result, exceptionSegment) => //TODO replace it with recursion to end faster
        result match {
          case Nil => Nil
          case _ =>
            result.dropRight(1) ++ excludingSegmentFromAnother(
              result.last,
              exceptionSegment
            )
        }
      }
  }

  private[core] def mergeSegmentsForDate(
      date: LocalDate,
      timeSegments: Seq[TimeSegment]
  ): Seq[TimeSegment] =
    mergeTimeSegments(timeSegments.filter(_.date.isEqual(date)))

  private[core] def mergeSegments(
      segments: Seq[TimeSegment]
  ): List[TimeSegment] = {
    segments
      .sortBy(_.startTime)
      .foldLeft(Nil: List[TimeSegment]) { (result, segment) =>
        result.dropRight(1) ++ result.lastOption
          .map(mergeTwoSegment(_, segment))
          .getOrElse(List(segment))
      }
  }

  private[core] def mergeSegments2(
      segments: Seq[TimeSegment]
  ): List[TimeSegment] = {
    segments
      .sortBy(_.startTime)
      .foldLeft(Nil: List[TimeSegment]) { (result, segment) =>
        result.lastOption
          .map { s =>
            mergeTwoSegmentOpt(s, segment) match {
              case (Some(_), Some(seg2)) => result :+ seg2
              case (Some(_), None)       => result
              case (None, Some(mergeResult)) =>
                result.dropRight(1) :+ mergeResult
              case _ =>
                result // normalement ca devrait pas arriver.. donc je suis pas fan :/
            }
          }
          .getOrElse(List(segment))
      }
  }

  // assuming seg1.startTime is always >= seg2.startTime ( and the same day )
  private[core] def mergeTwoSegment(
      seg1: TimeSegment,
      seg2: TimeSegment
  ): List[TimeSegment] = {
    if (seg2.startTime > seg1.endTime) {
      List(seg1, seg2)
    } else if (seg2.endTime < seg1.endTime) {
      List(seg1)
    } else {
      List(TimeSegment(seg1.date, Interval(seg1.startTime, seg2.endTime)))
    }
  }

  // assuming seg1.startTime is always >= seg2.startTime ( and the same day )
  private[core] def mergeTwoSegmentOpt(
      seg1: TimeSegment,
      seg2: TimeSegment
  ): (Option[TimeSegment], Option[TimeSegment]) = {
    if (seg2.startTime > seg1.endTime) {
      (Some(seg1), Some(seg2))
    } else if (seg2.endTime < seg1.endTime) {
      (Some(seg1), None)
    } else {
      (
        None,
        Some(TimeSegment(seg1.date, Interval(seg1.startTime, seg2.endTime)))
      )
    }
  }

  private[core] def excludingSegmentFromAnother(
      segment: TimeSegment,
      toExclude: TimeSegment
  ) = {
    if (segment.startTime >= toExclude.startTime && segment.endTime <= toExclude.endTime) {
      Nil
    } else if (segment.endTime <= toExclude.startTime || segment.startTime >= toExclude.endTime) {

      List(segment)

    } else if (segment.startTime < toExclude.startTime && segment.endTime <= toExclude.endTime) {

      List(
        TimeSegment(
          segment.date,
          Interval(segment.startTime, toExclude.startTime)
        )
      )

    } else if (segment.startTime >= toExclude.startTime && segment.endTime > toExclude.endTime) {

      List(
        TimeSegment(segment.date, Interval(toExclude.endTime, segment.endTime))
      )

    } else if (segment.startTime < toExclude.startTime && segment.endTime > toExclude.endTime) {
      List(
        TimeSegment(
          segment.date,
          Interval(segment.startTime, toExclude.startTime)
        ),
        TimeSegment(segment.date, Interval(toExclude.endTime, segment.endTime))
      )
    } else {
      println(s"should not append. segment: $segment, toExclude: $toExclude")
      Nil
    }
  }
}
