package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.{BusinessHoursByDayOfWeek, Interval, TimeSegment}

import scala.collection.mutable.ListBuffer

object Segments {

  import scala.math.Ordering.Implicits._

  def segmentsBetween(
      planning: BusinessHoursByDayOfWeek,
      planningTimeZone: ZoneId,
      exceptionSegments: List[TimeSegment]
  )(start: ZonedDateTime, end: ZonedDateTime): List[TimeSegment] = {
    def segmentsInStartDay(
        exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]],
        start: LocalDateTime
    ): ListBuffer[TimeSegment] =
      planning
        .getOrElse(start.getDayOfWeek, List.empty)
        .filter(_.endTime < start.toLocalTime)
        .foldLeft(ListBuffer.empty[TimeSegment]) { (result, interval) =>
          val newInterval =
            if (interval.startTime < start.toLocalTime) Interval(start.toLocalTime, interval.endTime)
            else interval

          result ++ splitTimeSegmentFromExceptions(
            exceptionsSegmentByDate,
            start.toLocalDate,
            TimeSegment(start.toLocalDate, newInterval)
          )
        }

    def segmentsInEndDay(
        exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]],
        end: LocalDateTime
    ): ListBuffer[TimeSegment] =
      planning
        .getOrElse(end.getDayOfWeek, List.empty)
        .filter(_.startTime > end.toLocalTime)
        .foldLeft(ListBuffer.empty[TimeSegment]) { (result, interval) =>
          val newInterval =
            if (interval.endTime > end.toLocalTime) Interval(interval.startTime, end.toLocalTime)
            else interval

          result ++ splitTimeSegmentFromExceptions(
            exceptionsSegmentByDate,
            end.toLocalDate,
            TimeSegment(end.toLocalDate, newInterval)
          )
        }

    def allSegmentsInDay(
        exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]],
        date: LocalDate
    ): List[TimeSegment] =
      planning
        .getOrElse(date.getDayOfWeek, List.empty)
        .flatMap(interval => splitTimeSegmentFromExceptions(exceptionsSegmentByDate, date, TimeSegment(date, interval)))

    val localStart                                          = start.withZoneSameInstant(planningTimeZone).toLocalDateTime
    val localEnd                                            = end.withZoneSameInstant(planningTimeZone).toLocalDateTime
    val exceptionsByDate: Map[LocalDate, List[TimeSegment]] = exceptionSegments.groupBy(_.date)

    if (localStart.toLocalDate == localEnd.toLocalDate) {
      segmentsInOneDay(
        planning,
        exceptionsByDate,
        localStart.toLocalDate,
        Interval(localStart.toLocalTime, localEnd.toLocalTime)
      )
    } else {
      val startDaySegments = segmentsInStartDay(exceptionsByDate, localStart)
      val endDaySegments   = segmentsInEndDay(exceptionsByDate, localEnd)

      val numberOfDays = Period.between(localStart.toLocalDate, localEnd.toLocalDate).getDays
      val dayRangeSegments =
        Range(1, numberOfDays)
          .foldLeft(ListBuffer.empty[TimeSegment]) { (allSegments, i) =>
            val date = localStart.plusDays(i.toLong)
            allSegments ++ allSegmentsInDay(exceptionsByDate, date.toLocalDate)
          }

      (startDaySegments ++ dayRangeSegments ++ endDaySegments).toList
    }
  }

  private def segmentsInOneDay(
      planning: BusinessHoursByDayOfWeek,
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]],
      date: LocalDate,
      query: Interval
  ): List[TimeSegment] =
    planning
      .getOrElse(date.getDayOfWeek, List.empty)
      .filter(interval => interval.endTime < query.startTime || interval.startTime > query.endTime) // isOutsideQuery
      .foldLeft(ListBuffer.empty[TimeSegment]) { (result, interval) =>
        val newInterval =
          if (interval.startTime < query.startTime && interval.endTime > query.endTime) // isInTheQuery
            Interval(query.startTime, query.endTime)
          else if (interval.startTime < query.startTime) // startsBeforeQuery
            Interval(query.startTime, interval.endTime)
          else if (interval.endTime > query.endTime) // endsAfterQuery
            Interval(interval.startTime, query.endTime)
          else
            interval

        result ++ splitTimeSegmentFromExceptions(exceptionsSegmentByDate, date, TimeSegment(date, newInterval))
      }
      .toList

  private[core] def splitTimeSegmentFromExceptions(
      exceptionsSegmentByDate: Map[LocalDate, List[TimeSegment]],
      date: LocalDate,
      initialSegment: TimeSegment
  ): List[TimeSegment] =
    exceptionsSegmentByDate
      .getOrElse(date, Nil)
      .sortBy(_.startTime)
      .foldLeft(List(initialSegment)) { (result, exceptionSegment) => // TODO replace it with recursion to end faster
        result match {
          case Nil => Nil // ???
          case _ =>
            result.dropRight(1) ++ excludingSegmentFromAnother(result.last, exceptionSegment) // TODO: The `dropRight(1)` is very very strange!
        }
      }

  private[core] def mergeSegments(
      segments: List[TimeSegment]
  ): List[TimeSegment] =
    segments
      .sortBy(_.startTime)
      .foldLeft(Nil: List[TimeSegment]) { (result, segment) =>
        result.dropRight(1) ++ // TODO: The `dropRight(1)` is very very strange!
          result.lastOption
            .map(mergeTwoSegment(_, segment))
            .getOrElse(List(segment))
      }

  private[core] def mergeSegments2(segments: List[TimeSegment]): List[TimeSegment] =
    segments
      .sortBy(_.startTime)
      .foldLeft(Nil: List[TimeSegment]) { (result, segment) =>
        result.lastOption
          .map { s =>
            mergeTwoSegmentOpt(s, segment) match {
              case (Some(_), Some(seg2))     => result :+ seg2
              case (Some(_), None)           => result
              case (None, Some(mergeResult)) => result.dropRight(1) :+ mergeResult
              case _                         => result // normalement ca devrait pas arriver.. donc je suis pas fan :/
            }
          }
          .getOrElse(List(segment))
      }

  // assuming seg1.startTime is always >= seg2.startTime ( and the same day )
  private[core] def mergeTwoSegment(seg1: TimeSegment, seg2: TimeSegment): List[TimeSegment] =
    if (seg2.startTime > seg1.endTime) List(seg1, seg2)
    else if (seg2.endTime < seg1.endTime) List(seg1)
    else List(TimeSegment(seg1.date, Interval(seg1.startTime, seg2.endTime)))

  // assuming seg1.startTime is always >= seg2.startTime ( and the same day )
  private[core] def mergeTwoSegmentOpt(
      seg1: TimeSegment,
      seg2: TimeSegment
  ): (Option[TimeSegment], Option[TimeSegment]) =
    if (seg2.startTime > seg1.endTime) (Some(seg1), Some(seg2))
    else if (seg2.endTime < seg1.endTime) (Some(seg1), None)
    else (None, Some(TimeSegment(seg1.date, Interval(seg1.startTime, seg2.endTime))))

  private[core] def excludingSegmentFromAnother(segment: TimeSegment, toExclude: TimeSegment): List[TimeSegment] =
    if (segment.startTime >= toExclude.startTime && segment.endTime <= toExclude.endTime)
      Nil
    else if (segment.endTime <= toExclude.startTime || segment.startTime >= toExclude.endTime)
      List(segment)
    else if (segment.startTime < toExclude.startTime && segment.endTime <= toExclude.endTime)
      TimeSegment(segment.date, Interval(segment.startTime, toExclude.startTime)) :: Nil
    else if (segment.startTime >= toExclude.startTime && segment.endTime > toExclude.endTime)
      TimeSegment(segment.date, Interval(toExclude.endTime, segment.endTime)) :: Nil
    else if (segment.startTime < toExclude.startTime && segment.endTime > toExclude.endTime)
      List(
        TimeSegment(segment.date, Interval(segment.startTime, toExclude.startTime)),
        TimeSegment(segment.date, Interval(toExclude.endTime, segment.endTime))
      )
    else {
      println(s"should not append. segment: $segment, toExclude: $toExclude") // TODO: To remove
      Nil
    }
}
