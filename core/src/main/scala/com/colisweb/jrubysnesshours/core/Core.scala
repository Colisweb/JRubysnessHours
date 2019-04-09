package com.colisweb.jrubysnesshours.core

import java.time._

object Core {

  final case class Interval(startTime: LocalTime, endTime: LocalTime)

  final case class BusinessHour(dayOfWeek: DayOfWeek, interval: Interval)

  final case class TimeSegment(date: LocalDate, interval: Interval) {
    val startTime: LocalTime = interval.startTime
    val endTime: LocalTime   = interval.endTime
  }

  type BusinessHoursByDayOfWeek = Map[DayOfWeek, List[Interval]]

  object BusinessHour {
    def toBusinessHoursForDayOfWeek(businessHours: List[BusinessHour]): BusinessHoursByDayOfWeek =
      businessHours.groupBy(_.dayOfWeek).mapValues(_.map(_.interval))
  }

  object TimeSegment {

    // TODO: Never used. To Remove ?
    private[core] def mergeTimeSegments(
        timeSegments: List[TimeSegment]
    ): List[TimeSegment] =
      timeSegments
        .sortBy(_.startTime)
        .foldLeft(List.empty[TimeSegment]) { (acc, segment) => // Merge exceptions if some overlap
          acc match { // always use preprend to simplify code here
            case Nil => segment +: acc
            case head :: tail =>
              segment.startTime compareTo head.endTime match {
                case r if r > 0 /* isAfter */  => segment +: acc
                case r if r < 0 /* isBefore */ => acc
                case _                         => TimeSegment(head.date, Interval(head.startTime, segment.endTime)) +: tail
              }
          }
        }
        .reverse //reverse because we use prepend in our fold --- // reverse or sortBy start ?

  }
}
