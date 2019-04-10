package com.colisweb.jrubysnesshours.core

import java.time._

object Core {

  case class TimeInterval(start: LocalTime, end: LocalTime)

  case class TimeIntervalForWeekDay(
      dayOfWeek: DayOfWeek,
      interval: TimeInterval
  )

  case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {

    def startTime: LocalTime = interval.start

    def endTime: LocalTime = interval.end
  }

  case class Schedule(
      planning: Map[DayOfWeek, List[TimeInterval]],
      exceptions: Map[LocalDate, List[TimeInterval]],
      timeZone: ZoneId
  )

  object Schedule {

//    def build(
//        plannings: List[TimeIntervalForWeekDay],
//        exceptions: List[TimeIntervalForDate],
//        timeZone: ZoneId
//    ): Schedule = {
//
//      Schedule(
//        plannings.groupBy(_.dayOfWeek).map {
//          case (dayOfWeek, intervals) =>
//            dayOfWeek -> ??? // flatten, sort and merge segments
//        },
//        exceptions.groupBy(_.date).map {
//          case (date, intervals) =>
//            date -> ??? // flatten, sort and merge segments
//        },
//        timeZone
//      )
//    }
  }

  object TimeIntervalForWeekDay {

    def toBusinessHoursForDayOfWeek(
        businessHours: List[TimeIntervalForWeekDay]
    ): Map[DayOfWeek, List[TimeInterval]] = {
      businessHours.groupBy(_.dayOfWeek).mapValues(_.map(_.interval))
    }
  }

  object TimeIntervalForDate {

    private[core] def mergeTimeSegments(
        timeSegments: Seq[TimeIntervalForDate]
    ): Seq[TimeIntervalForDate] =
      timeSegments
        .sortBy(_.startTime)
        .foldLeft(Nil: List[TimeIntervalForDate]) { (acc, segment) => // Merge exceptions if some overlap
          acc match { // always use preprend to simplify code here
            case Nil => segment +: acc
            case head :: tail => {
              if (segment.startTime.isAfter(head.endTime)) {
                segment +: acc
              } else if (segment.endTime.isBefore(head.endTime)) {
                acc
              } else {
                TimeIntervalForDate(
                  head.date,
                  TimeInterval(head.startTime, segment.endTime)
                ) +: tail
              }
            }
          }
        }
        .reverse //reverse because we use prepend in our fold --- // reverse or sortBy start ?
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
