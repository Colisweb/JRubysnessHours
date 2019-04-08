package com.colisweb.jrubysnesshours.core

import java.time._
import TimeOrderings._
import scala.math.Ordering.Implicits._

object Core {

  case class Interval(startTime: LocalTime, endTime: LocalTime)
  case class BusinessHour(dayOfWeek: DayOfWeek, interval: Interval)
  case class TimeSegment(
      date: LocalDate,
      startTime: LocalDateTime,
      endTime: LocalDateTime
  ) { // TODO replace with interval

    def splitFromExceptionTimeSegments(
        exceptionTimeSegments: Seq[TimeSegment]
    ): Seq[TimeSegment] = {
      val validExceptionSegments =
        TimeSegment.mergeTimeSegmentsForDate(this.date, exceptionTimeSegments)

      validExceptionSegments
        .foldLeft(List(this)) { (acc, exceptionSegment) =>
          acc match {
            case Nil => Nil
            case head :: tail =>
              if (head.startTime >= exceptionSegment.startTime && head.endTime <= exceptionSegment.endTime) {
                tail
              } else if (head.startTime < exceptionSegment.startTime && head.endTime < exceptionSegment.endTime) {

                TimeSegment(
                  head.date,
                  head.startTime,
                  exceptionSegment.startTime
                ) +: tail

              } else if (head.startTime > exceptionSegment.startTime && head.endTime > exceptionSegment.endTime) {

                TimeSegment(head.date, exceptionSegment.endTime, head.endTime) +: tail

              } else if (head.startTime < exceptionSegment.startTime && head.endTime > exceptionSegment.endTime) {
                List(
                  TimeSegment(
                    head.date,
                    exceptionSegment.endTime,
                    head.endTime
                  ), // the order here is reversed, as we use prepend everywhere in the fold
                  TimeSegment(
                    head.date,
                    head.startTime,
                    exceptionSegment.startTime
                  )
                ) ++ tail
              } else if (head.startTime < exceptionSegment.startTime && head.endTime > exceptionSegment.endTime) {
                acc
              } else {
                Nil // que faire ici ? On estime qu'on a gérer tous les cas ??
              }
          }
        }
        .reverse // reverse or sortBy start ?
    }
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

    private[core] def segmentBetween(
        start: LocalDateTime,
        end: LocalDateTime,
        businessHoursByDayOfWeek: BusinessHoursByDayOfWeek,
        intervalExceptions: Seq[TimeSegment] // TODO : maybe replace with a Map
    ): Seq[TimeSegment] = {

      // assuming that an intervalStart is always <= intervalEnd
      def intervalStartOnStartDay(
          intervalStartInDateTime: LocalDateTime,
          intervalEndInDateTime: LocalDateTime
      ): Option[LocalDateTime] =
        (
          start.compareTo(intervalStartInDateTime),
          start.compareTo(intervalEndInDateTime)
        ) match {
          case (_, y) if y >= 0 => None
          case (x, _) if x > 0  => Some(start)
          case _                => Some(intervalStartInDateTime)
        }

      // assuming that an intervalStart is always <= intervalEnd
      def intervalEndOnEndDay(
          intervalStartInDateTime: LocalDateTime,
          intervalEndInDateTime: LocalDateTime
      ): Option[LocalDateTime] =
        (
          end.compareTo(intervalStartInDateTime),
          end.compareTo(intervalEndInDateTime)
        ) match {
          case (x, _) if x <= 0 => None
          case (_, y) if y < 0  => Some(end)
          case _                => Some(intervalEndInDateTime)
        }

      def computeTimeSegmentStart(
          date: LocalDate,
          intervalStartInDateTime: LocalDateTime,
          intervalEndInDateTime: LocalDateTime
      ): Option[LocalDateTime] = {
        date.compareTo(start.toLocalDate) match {
          case x if x > 0 => Some(intervalStartInDateTime)
          case x if x == 0 =>
            intervalStartOnStartDay(
              intervalStartInDateTime,
              intervalEndInDateTime
            )
          case _ => None
        }
      }

      def computeTimeSegmentEnd(
          date: LocalDate,
          intervalStartInDateTime: LocalDateTime,
          intervalEndInDateTime: LocalDateTime
      ): Option[LocalDateTime] = {
        date.compareTo(end.toLocalDate) match {
          case x if x < 0 => Some(intervalEndInDateTime)
          case x if x == 0 =>
            intervalEndOnEndDay(intervalStartInDateTime, intervalEndInDateTime)
          case _ => None
        }
      }

      val numOfDays = Period
        .between(start.toLocalDate, end.toLocalDate)
        .getDays + 1

      Range(0, numOfDays)
        .map(i => start.plusDays(i.toLong))
        .flatMap { dateTime =>
          val date = dateTime.toLocalDate

          businessHoursByDayOfWeek
            .getOrElse(dateTime.getDayOfWeek, Nil)
            .flatMap { interval =>
              val intervalStartInDateTime = interval.startTime.atDate(date)
              val intervalEndInDateTime = interval.endTime.atDate(date)

              for {
                startSegment <- computeTimeSegmentStart(
                  date,
                  intervalStartInDateTime,
                  intervalEndInDateTime
                )
                endSegment <- computeTimeSegmentEnd(
                  date,
                  intervalStartInDateTime,
                  intervalEndInDateTime
                )
              } yield TimeSegment(date, startSegment, endSegment)
            }

        }
        .flatMap(_.splitFromExceptionTimeSegments(intervalExceptions))
    }

    private[core] def mergeTimeSegmentsForDate(
        date: LocalDate,
        timeSegments: Seq[TimeSegment]
    ): Seq[TimeSegment] =
      mergeTimeSegments(timeSegments.filter(_.date.isEqual(date)))

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
                TimeSegment(head.date, head.startTime, segment.endTime) +: tail
              }
            }
          }
        }
        .reverse //reverse because we use prepend in our fold --- // reverse or sortBy start ?
  }

}
