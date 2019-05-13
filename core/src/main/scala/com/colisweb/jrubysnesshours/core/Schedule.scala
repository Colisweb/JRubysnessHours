package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time._
import java.time.temporal.ChronoUnit.DAYS

import com.colisweb.jrubysnesshours.core.Schedule._
import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._

final case class Schedule private[core] (
    planning: Map[DayOfWeek, List[TimeInterval]],
    exceptions: Map[LocalDate, List[TimeInterval]],
    timeZone: ZoneId
) {

  def splitTimeSegmentsSingleDate(date: LocalDate, hours: Long): List[TimeInterval] =
    splitTimeSegments(zoned(date.atTime(MIN)), zoned(date.atTime(MAX)), hours).map(_.interval)

  def splitTimeSegments(
      start: ZonedDateTime,
      end: ZonedDateTime,
      hours: Long,
      cutOff: Option[DoubleCutOff] = None
  ): List[TimeIntervalForDate] = {
    val startTime = local(start).toLocalTime

    for {
      nextWorkingDay <- nextOpenTimeAfter(start.plusDays(1).withHour(0).withMinute(0)).toList
      localStart = cutOff.fold(start.toLocalDateTime)(
        _.nextAvailableMoment(startTime, start.toLocalDate, nextWorkingDay.toLocalDate)
      )
      interval <- intervalsBetween(localStart, local(end))
      rounded  <- interval.roundToFullHours
      segment  <- rounded.split(hours)
    } yield segment
  }

  @inline def planningFor(dayOfWeek: DayOfWeek): List[TimeInterval] = planning.getOrElse(dayOfWeek, Nil)
  @inline def exceptionFor(date: LocalDate): List[TimeInterval]     = exceptions.getOrElse(date, Nil)

  def intervalsBetween(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] =
    intervalsBetween(local(start), local(end))

  private def intervalsBetween(localStart: LocalDateTime, localEnd: LocalDateTime): List[TimeIntervalForDate] =
    if (localStart < localEnd) intervalsBetween(DateTimeInterval(localStart, localEnd))
    else Nil

  private def intervalsBetween(interval: DateTimeInterval): List[TimeIntervalForDate] =
    interval.asSameDateInterval.fold(intervalsInTwoDates(interval.start, interval.end))(intervalsInSameDate)

  def contains(instant: ZonedDateTime): Boolean = {
    val localInstant = local(instant)
    val time         = localInstant.toLocalTime

    @inline def existsPlanning =
      planningFor(localInstant.getDayOfWeek).exists(_.contains(time))

    @inline def notExistsException =
      !exceptionFor(localInstant.toLocalDate).exists(_.contains(time))

    existsPlanning && notExistsException
  }

  def contains(start: ZonedDateTime, end: ZonedDateTime): Boolean = {
    assert(
      isTheSameDayForZone(start, end, timeZone),
      s"start $start and end $end are not the same day for zone $timeZone"
    )

    val startLocalDate = start.toLocalDate

    val interval = TimeInterval(
      start = start.withZoneSameInstant(timeZone).toLocalTime,
      end = end.withZoneSameInstant(timeZone).toLocalTime
    )
    @inline def existsPlanning =
      planningFor(startLocalDate.getDayOfWeek).exists(_.encloses(interval))

    @inline def notExistsException =
      exceptionFor(startLocalDate).forall { exception =>
        !exception.isConnected(interval) || exception.isBefore(interval) || exception.isAfter(interval)
      }

    existsPlanning && notExistsException
  }

  def nextOpenTimeAfter(instant: ZonedDateTime): Option[ZonedDateTime] = {
    @tailrec
    def findNextOpenTimeAfter(date: LocalDate, additionalException: List[TimeInterval]): Option[LocalDateTime] =
      allIntervalsInDate(date, additionalException) match {
        case interval :: _ => Some(LocalDateTime.of(date, interval.start))
        case Nil           => findNextOpenTimeAfter(date.plusDays(1), Nil)
      }

    if (planning.nonEmpty) {
      val localInstant = local(instant)
      val date         = localInstant.toLocalDate
      val time         = localInstant.toLocalTime
      findNextOpenTimeAfter(date, startOfDayCut(time)).map(_.atZone(instant.getZone))
    } else None
  }

  private def intervalsInStartDay(start: LocalDateTime): List[TimeIntervalForDate] =
    allIntervalsInDate(start.toLocalDate, startOfDayCut(start.toLocalTime))

  private def intervalsInEndDay(end: LocalDateTime): List[TimeIntervalForDate] =
    allIntervalsInDate(end.toLocalDate, endOfDayCut(end.toLocalTime))

  private def intervalsInSameDate(timeIntervalForDate: TimeIntervalForDate): List[TimeIntervalForDate] =
    allIntervalsInDate(
      timeIntervalForDate.date,
      startOfDayCut(timeIntervalForDate.start) ++ endOfDayCut(timeIntervalForDate.end)
    )

  private def intervalsInTwoDates(localStart: LocalDateTime, localEnd: LocalDateTime): List[TimeIntervalForDate] = {
    val localStartDate = localStart.toLocalDate
    val days           = localStartDate.until(localEnd.toLocalDate, DAYS)

    val intervalsInIntermediateDays = (1L until days).flatMap { days =>
      allIntervalsInDate(localStartDate.plusDays(days))
    }

    intervalsInStartDay(localStart) ++ intervalsInIntermediateDays ++ intervalsInEndDay(localEnd)
  }

  private def allIntervalsInDate(
      date: LocalDate,
      additionalExceptions: List[TimeInterval] = Nil
  ): List[TimeIntervalForDate] =
    cutExceptions(planningFor(date.getDayOfWeek), additionalExceptions ::: exceptionFor(date))
      .map(interval => TimeIntervalForDate(date = date, interval = interval))

  private def local(instant: ZonedDateTime): LocalDateTime = instant.withZoneSameInstant(timeZone).toLocalDateTime
  private def zoned(local: LocalDateTime): ZonedDateTime   = local.atZone(timeZone)
}

object Schedule {

  def apply(
      planning: List[TimeIntervalForWeekDay],
      exceptions: List[DateTimeInterval],
      timeZone: ZoneId
  ): Schedule = {
    def mergeIntervals(intervals: List[TimeInterval]): List[TimeInterval] = {
      def mergeTwoIntervals(interval1: TimeInterval, interval2: TimeInterval): List[TimeInterval] =
        if (interval1 isBefore interval2) List(interval1, interval2)
        else if (interval1 encloses interval2) List(interval1)
        else List(interval1 union interval2)

      intervals
        .sortBy(_.start)
        .foldRight(List.empty[TimeInterval]) {
          case (interval, h :: t) => mergeTwoIntervals(interval, h) ::: t // TODO: `:::` is not in constant time.
          case (interval, Nil)    => List(interval)
        }
    }

    def dateTimeIntervalsToExceptions: List[TimeIntervalForDate] = {
      exceptions
        .flatMap { dateTimeInterval: DateTimeInterval =>
          val localStartTime = dateTimeInterval.start.toLocalTime
          val localEndTime   = dateTimeInterval.end.toLocalTime

          val localStartDate = dateTimeInterval.start.toLocalDate
          val localEndDate   = dateTimeInterval.end.toLocalDate

          val numberOfDays = localStartDate.until(localEndDate, DAYS)

          if (numberOfDays == 0L) {
            val newInterval = TimeInterval(start = localStartTime, end = localEndTime)
            List(TimeIntervalForDate(date = localStartDate, interval = newInterval))
          } else {
            val midDays =
              (1L until numberOfDays).map { i =>
                val date        = localStartDate.plusDays(i)
                val newInterval = TimeInterval(start = MIN, end = MAX)
                TimeIntervalForDate(date = date, interval = newInterval)
              }

            val firstDay =
              if (localStartTime < MAX) {
                ListBuffer(
                  TimeIntervalForDate(
                    date = localStartDate,
                    interval = TimeInterval(start = localStartTime, end = MAX)
                  )
                )
              } else ListBuffer.empty

            val lastDay =
              if (MIN < localEndTime)
                ListBuffer(
                  TimeIntervalForDate(
                    date = localEndDate,
                    interval = TimeInterval(start = MIN, end = localEndTime)
                  )
                )
              else ListBuffer.empty

            firstDay ++ midDays ++ lastDay
          }
        }
    }

    import utils.GroupableOps._

    Schedule(
      planning = planning.groupMap(_.dayOfWeek)(_.interval).mapValues(mergeIntervals),
      exceptions = dateTimeIntervalsToExceptions.groupMap(_.date)(_.interval).mapValues(mergeIntervals),
      timeZone = timeZone
    )
  }

  def isTheSameDayForZone(time1: ZonedDateTime, time2: ZonedDateTime, zone: ZoneId): Boolean =
    time1.withZoneSameInstant(zone).toLocalDate == time2.withZoneSameInstant(zone).toLocalDate

  private[core] def cutExceptions(intervals: List[TimeInterval], exceptions: List[TimeInterval]): List[TimeInterval] =
    intervals.flatMap { interval =>
      exceptions.foldLeft(List(interval)) { (acc, exception) =>
        acc.flatMap(_ diff exception)
      }
    }

  private[core] def startOfDayCut(start: LocalTime): List[TimeInterval] =
    if (start == MIN) Nil
    else List(TimeInterval(start = MIN, end = start))

  private[core] def endOfDayCut(end: LocalTime): List[TimeInterval] =
    if (end == MAX) Nil
    else List(TimeInterval(start = end, end = MAX))
}
