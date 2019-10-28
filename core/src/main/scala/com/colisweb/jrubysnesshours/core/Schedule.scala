package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time._
import java.time.temporal.ChronoUnit.DAYS

import com.colisweb.jrubysnesshours.core.Schedule._
import com.colisweb.jrubysnesshours.core.TimeInterval.{cutStartOfDay, toMap}
import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

final case class Schedule(
    planning: Map[DayOfWeek, List[TimeInterval]],
    exceptions: Map[LocalDate, List[TimeInterval]],
    timeZone: ZoneId
) {

  def splitTimeSegmentsSingleDate(date: LocalDate, hours: Int): List[TimeInterval] =
    splitTimeSegments(zoned(date.atTime(MIN)), zoned(date.atTime(MAX)), Duration.ofHours(hours.toLong)).map(_.interval)

  def splitTimeSegments(
      start: ZonedDateTime,
      end: ZonedDateTime,
      duration: Duration,
      cutOff: Option[DoubleCutOff] = None
  ): List[TimeIntervalForDate] = {
    val startTime = local(start).toLocalTime

    for {
      nextWorkingDay <- nextOpenTimeAfter(start.plusDays(1).truncatedTo(DAYS)).toList
      localStart = cutOff.fold(local(start))(
        _.nextAvailableMoment(startTime, start.toLocalDate, nextWorkingDay.toLocalDate)
      )
      interval <- intervalsBetween(localStart, local(end))
      segment  <- interval.split(duration)
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
    interval.asSameDateIntervals.flatMap(allIntervalsInDate)

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
      !exceptionFor(startLocalDate).exists(_.isConnectedStrict(interval))

    existsPlanning && notExistsException
  }

  def nextOpenTimeAfter(zdt: ZonedDateTime): Option[ZonedDateTime] =
    if (planning.nonEmpty) {
      val instant = local(zdt)
      findNextOpenTimeAfter(instant.toLocalDate, cutStartOfDay(instant.toLocalTime)).map(_.atZone(zdt.getZone))
    } else None

  @tailrec
  private def findNextOpenTimeAfter(
      date: LocalDate,
      additionalException: Iterable[TimeInterval]
  ): Option[LocalDateTime] =
    allIntervalsInDate(date, additionalException).headOption match {
      case Some(interval) => Some(LocalDateTime.of(date, interval.start))
      case None           => findNextOpenTimeAfter(date.plusDays(1), Nil)
    }

  private def allIntervalsInDate(interval: TimeIntervalForDate): List[TimeIntervalForDate] = {
    cutExceptions(planningFor(interval.date.getDayOfWeek), interval.cutBoth ::: exceptionFor(interval.date))
      .map(i => TimeIntervalForDate(date = interval.date, interval = i))
  }

  private def allIntervalsInDate(
      date: LocalDate,
      additionalExceptions: Iterable[TimeInterval]
  ): List[TimeIntervalForDate] =
    cutExceptions(planningFor(date.getDayOfWeek), exceptionFor(date) ++ additionalExceptions)
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

    Schedule(
      planning = toMap(planning)(_.dayOfWeek, _.interval),
      exceptions = toMap(exceptions.flatMap(_.splitByDates))(_.date, _.interval),
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

}
