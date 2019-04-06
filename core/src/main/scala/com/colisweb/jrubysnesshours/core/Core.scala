package com.colisweb.jrubysnesshours.core

import java.time._
import java.util.Date

object Core {

  case class Interval(startTime: LocalTime, endTime: LocalTime)
  case class BusinessHour(dayOfWeek: DayOfWeek, interval: Interval)
  case class TimeSegment(date: LocalDate, start_time: LocalDateTime, end_time: LocalDateTime)

  type BusinessHoursByDayOfWeek = Map[DayOfWeek, List[Interval]]

  object BusinessHour {

    def toBusinessHoursForDayOfWeek(businessHours: List[BusinessHour]): BusinessHoursByDayOfWeek = {
      businessHours.groupBy(_.dayOfWeek).mapValues(_.map(_.interval))
    }
  }

  object TimeSegment {
    def segmentBetween(
                        start: LocalDateTime,
                        end: LocalDateTime,
                        businessHoursByDayOfWeek: BusinessHoursByDayOfWeek,
                        intervalExceptions: Seq[TimeSegment]
    ): Seq[TimeSegment] = {

      // assuming that an intervalStart is always <= intervalEnd
      def intervalStartOnStartDay(intervalStartInDateTime: LocalDateTime, intervalEndInDateTime: LocalDateTime): Option[LocalDateTime] =
        (start.compareTo(intervalStartInDateTime), start.compareTo(intervalEndInDateTime)) match {
          case (_, y) if y >= 0 => None
          case (x, _) if x > 0 => Some(start)
          case _ => Some(intervalStartInDateTime)
      }

      // assuming that an intervalStart is always <= intervalEnd
      def intervalEndOnEndDay(intervalStartInDateTime: LocalDateTime, intervalEndInDateTime: LocalDateTime): Option[LocalDateTime] =
        (end.compareTo(intervalStartInDateTime), end.compareTo(intervalEndInDateTime)) match {
          case (x, _) if x <= 0 => None
          case (_, y) if y < 0 => Some(end)
          case _ => Some(intervalEndInDateTime)
        }

      def computeTimeSegmentStart(date: LocalDate, intervalStartInDateTime: LocalDateTime, intervalEndInDateTime: LocalDateTime): Option[LocalDateTime] = {
        date.compareTo(start.toLocalDate) match {
          case x if x > 0 => Some(intervalStartInDateTime)
          case x if x == 0 => intervalStartOnStartDay(intervalStartInDateTime, intervalEndInDateTime)
          case _ => None
        }
      }

      def computeTimeSegmentEnd(date: LocalDate, intervalStartInDateTime: LocalDateTime, intervalEndInDateTime: LocalDateTime): Option[LocalDateTime] = {
        date.compareTo(end.toLocalDate) match {
          case x if x < 0 => Some(intervalEndInDateTime)
          case x if x == 0 => intervalEndOnEndDay(intervalStartInDateTime, intervalEndInDateTime)
          case _ => None
        }
      }

      /*Range(0, numOfDays)
        .map(i => start.plusDays(i.toLong))
        .flatMap { rangeDateTime =>
          val rangeDate = rangeDateTime.toLocalDate
          businessHoursByDayOfWeek(rangeDateTime.getDayOfWeek)
            .flatMap { interval =>

              val intervalStartInDateTime = interval.startTime.atDate(rangeDate)
              val intervalEndInDateTime = interval.endTime.atDate(rangeDate)

              for {
                startSegment <- computeTimeSegmentStart(rangeDateTime, intervalStartInDateTime, intervalEndInDateTime)
                endSegment <- computeTimeSegmentEnd(rangeDateTime, intervalStartInDateTime, intervalEndInDateTime)
              } yield (startSegment, endSegment)

            }
        }*/

      val period = Period.between(start.toLocalDate, end.toLocalDate)
      val numOfDays = period.getDays + 1

      println(intervalExceptions)

      (for {
        dateTime <- Range(0, numOfDays).map(i => start.plusDays(i.toLong))
        date = dateTime.toLocalDate
        interval <- businessHoursByDayOfWeek.getOrElse(dateTime.getDayOfWeek, Nil)
        intervalStartInDateTime = interval.startTime.atDate(date)
        intervalEndInDateTime = interval.endTime.atDate(date)
      } yield {
        for {
          startSegment <- computeTimeSegmentStart(date, intervalStartInDateTime, intervalEndInDateTime)
          endSegment <- computeTimeSegmentEnd(date, intervalStartInDateTime, intervalEndInDateTime)
        } yield TimeSegment(date, startSegment, endSegment)
      })
        .map() // traiter les intervalException ici ?
        .flatten
        .map
    }
  }

}
