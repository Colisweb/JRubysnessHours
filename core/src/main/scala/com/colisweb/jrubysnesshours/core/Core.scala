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
}
