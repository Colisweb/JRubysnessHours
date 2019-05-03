package com.colisweb.jrubysnesshours.core

import java.time._
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._

final case class Schedule private[core] (
    planning: Map[DayOfWeek, List[TimeInterval]],
    exceptions: Map[LocalDate, List[TimeInterval]],
    timeZone: ZoneId
) {
  @inline def planningFor(dayOfWeek: DayOfWeek): List[TimeInterval] = planning.getOrElse(dayOfWeek, Nil)
  @inline def exceptionFor(date: LocalDate): List[TimeInterval]     = exceptions.getOrElse(date, Nil)

  def intervalsBetween(start: ZonedDateTime, end: ZonedDateTime): List[TimeIntervalForDate] = {
    val localStart     = local(start)
    val localEnd       = local(end)
    val localStartDate = localStart.toLocalDate
    val localEndDate   = localEnd.toLocalDate

    if (localStartDate == localEndDate) {
      intervalsInSameDay(localStartDate, TimeInterval(localStart.toLocalTime, localEnd.toLocalTime))
    } else {
      val startDayIntervals: List[TimeIntervalForDate] = intervalsInStartDay(localStart)
      val endDayIntervals: List[TimeIntervalForDate]   = intervalsInEndDay(localEnd)

      val numberOfDays = localStartDate.until(localEndDate, ChronoUnit.DAYS)

      val dayRangeIntervals: ListBuffer[TimeIntervalForDate] =
        (1L until numberOfDays)
          .foldLeft(ListBuffer.empty[TimeIntervalForDate]) { (acc, i) =>
            val date = localStartDate.plusDays(i)
            acc ++ allIntervalsInDate(date)
          }

      startDayIntervals ++ dayRangeIntervals ++ endDayIntervals
    }
  }

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
    assert(Schedule.isTheSameDayForZone(start, end, timeZone))

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
    if (planning.nonEmpty) {

      @tailrec
      def findNextOpenTimeAfter(date: LocalDate, additionalException: List[TimeInterval]): Option[LocalDateTime] =
        allIntervalsInDate(date, additionalException) match {
          case interval :: _ => Some(LocalDateTime.of(date, interval.start))
          case Nil           => findNextOpenTimeAfter(date.plusDays(1), Nil)
        }

      val localInstant = local(instant)
      val date         = localInstant.toLocalDate
      val time         = localInstant.toLocalTime

      findNextOpenTimeAfter(date, startOfDayCut(time))
        .map(_.atZone(instant.getZone))

    } else None
  }

  private[core] def startOfDayCut(start: LocalTime): List[TimeInterval] =
    if (start == LocalTime.MIN) Nil
    else List(TimeInterval(start = LocalTime.MIN, end = start))

  private[core] def endOfDayCut(end: LocalTime): List[TimeInterval] =
    if (end == LocalTime.MAX) Nil
    else List(TimeInterval(start = end, end = LocalTime.MAX))

  private[core] def intervalsInStartDay(start: LocalDateTime): List[TimeIntervalForDate] =
    allIntervalsInDate(start.toLocalDate, startOfDayCut(start.toLocalTime))

  private[core] def intervalsInEndDay(end: LocalDateTime): List[TimeIntervalForDate] =
    allIntervalsInDate(end.toLocalDate, endOfDayCut(end.toLocalTime))

  private[core] def intervalsInSameDay(date: LocalDate, query: TimeInterval): List[TimeIntervalForDate] =
    allIntervalsInDate(date, startOfDayCut(query.start) ++ endOfDayCut(query.end))

  private[core] def allIntervalsInDate(
      date: LocalDate,
      additionalExceptions: List[TimeInterval] = Nil
  ): List[TimeIntervalForDate] =
    Schedule
      .cutExceptions(planningFor(date.getDayOfWeek), additionalExceptions ::: exceptionFor(date))
      .map(interval => TimeIntervalForDate(date = date, interval = interval))

  private def local(instant: ZonedDateTime): LocalDateTime = instant.withZoneSameInstant(timeZone).toLocalDateTime
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

          val numberOfDays = localStartDate.until(localEndDate, ChronoUnit.DAYS)

          if (numberOfDays == 0L) {
            val newInterval = TimeInterval(start = localStartTime, end = localEndTime)
            List(TimeIntervalForDate(date = localStartDate, interval = newInterval))
          } else {
            val midDays =
              (1L until numberOfDays).map { i =>
                val date        = localStartDate.plusDays(i)
                val newInterval = TimeInterval(start = LocalTime.MIN, end = LocalTime.MAX)
                TimeIntervalForDate(date = date, interval = newInterval)
              }

            val firstDay =
              if (localStartTime < LocalTime.MAX) {
                ListBuffer(
                  TimeIntervalForDate(
                    date = localStartDate,
                    interval = TimeInterval(start = localStartTime, end = LocalTime.MAX)
                  )
                )
              } else ListBuffer.empty

            val lastDay =
              if (LocalTime.MIN < localEndTime)
                ListBuffer(
                  TimeIntervalForDate(
                    date = localEndDate,
                    interval = TimeInterval(start = LocalTime.MIN, end = localEndTime)
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
        acc.flatMap(_ minus exception)
      }
    }
}
