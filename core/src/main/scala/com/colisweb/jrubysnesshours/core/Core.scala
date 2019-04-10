package com.colisweb.jrubysnesshours.core

import java.time._
import scala.math.Ordering.Implicits._

object Core {

  case class Schedule(
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]],
      timeZone: ZoneId
  )

  object Schedule {

    def build(
        plannings: List[TimeIntervalForWeekDay],
        exceptions: List[TimeIntervalForDate],
        timeZone: ZoneId
    ): Schedule = {

      def mergeTwoIntervals(
          interval1: TimeInterval,
          interval2: TimeInterval
      ): List[TimeInterval] = {
        if (interval2.start > interval1.end) {
          List(interval1, interval2)
        } else if (interval2.end < interval1.end) {
          List(interval1)
        } else {
          List(TimeInterval(interval1.start, interval2.end))
        }
      }

      def prepareWeekDayIntervals(
          intervals: List[TimeIntervalForWeekDay]
      ): List[TimeInterval] =
        intervals
          .sortBy(_.interval.start)
          .foldRight(List.empty[TimeInterval]) {
            case (dayInterval, h :: t) =>
              mergeTwoIntervals(dayInterval.interval, h) ::: t
            case (dayInterval, Nil) => List(dayInterval.interval)
          }

      def prepareDateIntervals(
          intervals: List[TimeIntervalForDate]
      ): List[TimeInterval] =
        intervals
          .sortBy(_.interval.start)
          .foldRight(List.empty[TimeInterval]) {
            case (dateInterval, h :: t) =>
              mergeTwoIntervals(dateInterval.interval, h) ::: t
            case (dateInterval, Nil) => List(dateInterval.interval)
          }

      Schedule(
        plannings.groupBy(_.dayOfWeek).map {
          case (dayOfWeek, intervals) =>
            dayOfWeek -> prepareWeekDayIntervals(intervals)
        },
        exceptions.groupBy(_.date).map {
          case (date, intervals) => date -> prepareDateIntervals(intervals)
        },
        timeZone
      )
    }
  }

  def within(
      schedule: Schedule
  )(start: ZonedDateTime, end: ZonedDateTime): Duration = {
    Segments
      .segmentsBetween(schedule)(start, end)
      .foldLeft(Duration.ZERO)(
        (total, segment) =>
          total.plus(Duration.between(segment.startTime, segment.endTime))
      )
  }

  def isOpenForDurationInDate(
      schedule: Schedule
  )(date: LocalDate, duration: Duration): Boolean = {

    val start = ZonedDateTime.of(date, LocalTime.MIN, schedule.timeZone)
    val end = ZonedDateTime.of(date, LocalTime.MAX, schedule.timeZone)

    Segments
      .segmentsBetween(schedule)(start, end)
      .exists(
        segment =>
          Duration
            .between(segment.startTime, segment.endTime)
            .compareTo(duration) >= 0
      )
  }

  def isOpen(schedule: Schedule)(instant: ZonedDateTime): Boolean = {

    Segments
      .segmentsBetween(schedule)(instant, instant)
      .nonEmpty
  }
}
