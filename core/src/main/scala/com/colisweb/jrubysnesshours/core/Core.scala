package com.colisweb.jrubysnesshours.core

import java.time._
import TimeOrderings._

object Core {



  case class Interval(startTime: LocalTime, endTime: LocalTime)
  case class BusinessHour(dayOfWeek: DayOfWeek, interval: Interval)
  case class TimeSegment(date: LocalDate, startTime: LocalDateTime, endTime: LocalDateTime) {

    def splitFromExceptionTimeSegments(exceptionTimeSegments: Seq[TimeSegment]): Seq[TimeSegment] = {
      val validExceptionSegments = TimeSegment.mergeTimeSegmentsForDate(this.date, exceptionTimeSegments)

      validExceptionSegments.foldLeft(List(this)) { (acc, exceptionSegment) =>
        acc match {
          case Nil => Nil
          case head :: tail =>

            val startComp = head.startTime.compareTo(exceptionSegment.startTime)
            val endComp = head.endTime.compareTo(exceptionSegment.endTime)

            if (startComp >= 0 && endComp <= 0) {
              tail
            } else if (startComp < 0 && endComp < 0) {
              TimeSegment(head.date, head.startTime, exceptionSegment.startTime) +: tail
            } else if (startComp > 0 && endComp > 0) {
              TimeSegment(head.date, exceptionSegment.endTime, head.endTime) +: tail
            } else if (startComp < 0 && endComp > 0) {
              List(
                TimeSegment(head.date, exceptionSegment.endTime, head.endTime), // the order here is reversed, as we use prepend everywhere in the fold
                TimeSegment(head.date, head.startTime, exceptionSegment.startTime)
              ) ++ tail
            } else if (head.startTime.isBefore(exceptionSegment.startTime) && head.endTime.isAfter(exceptionSegment.endTime)) {
              acc
            } else {
              Nil // que faire ici ? On estime qu'on a gérer tous les cas ??
            }
        }
      }.reverse // reverse or sortBy start ?
    }
  }

  type BusinessHoursByDayOfWeek = Map[DayOfWeek, List[Interval]]

  object BusinessHour {

    def toBusinessHoursForDayOfWeek(businessHours: List[BusinessHour]): BusinessHoursByDayOfWeek = {
      businessHours.groupBy(_.dayOfWeek).mapValues(_.map(_.interval))
    }
  }

  object TimeSegment {
    private[core] def segmentBetween(
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

      (
        for {
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
        }
      )
        .flatten
        .flatMap(_.splitFromExceptionTimeSegments(intervalExceptions))
    }

    private[core] def mergeTimeSegmentsForDate(date: LocalDate, timeSegments: Seq[TimeSegment]): Seq[TimeSegment] =
      mergeTimeSegments(timeSegments.filter(_.date.isEqual(date)))


    private[core] def mergeTimeSegments(timeSegments: Seq[TimeSegment]): Seq[TimeSegment] =
      timeSegments
        .sortBy(_.startTime)
        .foldLeft(Nil: List[TimeSegment]) { (acc, exceptionSegment) => // Merge exceptions if some overlap
          acc match { // always use preprend to simplify code here
            case Nil => exceptionSegment +: acc
            case head :: tail => {
              if (exceptionSegment.startTime.isAfter(head.endTime)) {
                exceptionSegment +: acc
              } else {
                TimeSegment(head.date, head.startTime, exceptionSegment.endTime) +: tail
              }
            }
          }
        }.reverse //reverse because we use prepend in our fold --- // reverse or sortBy start ?
  }

}
