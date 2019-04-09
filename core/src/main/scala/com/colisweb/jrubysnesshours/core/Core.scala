package com.colisweb.jrubysnesshours.core

import java.time._

object Core {

  case class Interval(startTime: LocalTime, endTime: LocalTime)

  case class BusinessHour(dayOfWeek: DayOfWeek, interval: Interval)

  case class TimeSegment(date: LocalDate, interval: Interval) {

    def startTime: LocalTime = interval.startTime

    def endTime: LocalTime = interval.endTime
  }

  type BusinessHoursByDayOfWeek = Map[DayOfWeek, List[Interval]]

  object BusinessHour {

    def toBusinessHoursForDayOfWeek(
        businessHours: List[BusinessHour]
    ): BusinessHoursByDayOfWeek = {
      businessHours.groupBy(_.dayOfWeek).mapValues(_.map(_.interval))
    }
  }

  object TimeSegment {

    private[core] def mergeTimeSegments(
        timeSegments: Seq[TimeSegment]
    ): Seq[TimeSegment] =
      timeSegments
        .sortBy(_.startTime)
        .foldLeft(Nil: List[TimeSegment]) { (acc, segment) => // Merge exceptions if some overlap
          acc match { // always use preprend to simplify code here
            case Nil => segment +: acc
            case head :: tail => {
              if (segment.startTime.isAfter(head.endTime)) {
                segment +: acc
              } else if (segment.endTime.isBefore(head.endTime)) {
                acc
              } else {
                TimeSegment(
                  head.date,
                  Interval(head.startTime, segment.endTime)
                ) +: tail
              }
            }
          }
        }
        .reverse //reverse because we use prepend in our fold --- // reverse or sortBy start ?
  }

  def within(
      planning: BusinessHoursByDayOfWeek,
      planningTimeZone: ZoneId,
      exceptionSegments: List[TimeSegment]
  )(start: ZonedDateTime, end: ZonedDateTime): Duration = {
    Segments
      .segmentsBetween(planning, planningTimeZone, exceptionSegments)(
        start,
        end
      )
      .foldLeft(Duration.ZERO)(
        (total, segment) =>
          total.plus(Duration.between(segment.startTime, segment.endTime))
      )
  }

  def isOpenForDurationInDate(
      planning: BusinessHoursByDayOfWeek,
      exceptionSegments: List[TimeSegment]
  )(date: LocalDate, duration: Duration): Boolean = {

    val start = LocalDateTime.of(date, LocalTime.MIN)
    val end = LocalDateTime.of(date, LocalTime.MAX)

    Segments
      .segmentsBetween(planning, exceptionSegments)(start, end)
      .exists(
        segment =>
          Duration
            .between(segment.startTime, segment.endTime)
            .compareTo(duration) >= 0
      )
  }

  def isOpen(
      planning: BusinessHoursByDayOfWeek,
      planningTimeZone: ZoneId,
      exceptionSegments: List[TimeSegment]
  )(instant: ZonedDateTime): Boolean = {

    Segments
      .segmentsBetween(planning, planningTimeZone, exceptionSegments)(
        instant,
        instant
      )
      .nonEmpty
  }
}
