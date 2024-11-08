package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._
import java.time._

import com.colisweb.jrubysnesshours.core._
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule._

final class JRubyzSchedule private[jruby] (private[jruby] val schedule: Schedule) {
  def splitTimeSegments(
      startsAt: ZonedDateTime,
      endsAt: ZonedDateTime,
      minutes: Int,
      cutOff: Option[DoubleCutOff] = None
  ): Array[RubyTimeSegmentInterval] =
    schedule
      .splitTimeSegments(startsAt, endsAt, Duration.ofMinutes(minutes.toLong), cutOff)
      .map(RubyTimeSegmentInterval(_, schedule.timeZone))
      .toArray

  def splitTimeSegments(
      startsAt: ZonedDateTime,
      endsAt: ZonedDateTime,
      duration: Duration,
      cutOff: Option[DoubleCutOff]
  ): Array[RubyTimeSegmentInterval] =
    schedule
      .splitTimeSegments(startsAt, endsAt, duration, cutOff)
      .map(RubyTimeSegmentInterval(_, schedule.timeZone))
      .toArray

  def timeSegments(startsAt: ZonedDateTime, endsAt: ZonedDateTime): Array[RubyTimeSegmentInterval] =
    schedule
      .intervalsBetween(startsAt, endsAt)
      .map(RubyTimeSegmentInterval(_, schedule.timeZone))
      .toArray

  def contains(start: ZonedDateTime, end: ZonedDateTime): Boolean = {
    schedule.contains(start = start, end = end)
  }

  def isOpen(time: ZonedDateTime): Boolean = schedule.contains(time)

  def nextOpentime(time: ZonedDateTime): ZonedDateTime =
    schedule
      .nextOpenTimeAfter(time)
      .map(_.withZoneSameInstant(UTC))
      .orNull
}

object JRubyzSchedule {
  val UTC: ZoneId = ZoneId.of("UTC")

  def exception(startsAtZonedDateTime: String, endsAtZonedDateTime: String): DateTimeInterval =
    exception(ZonedDateTime.parse(startsAtZonedDateTime), ZonedDateTime.parse(endsAtZonedDateTime))

  def exception(startsAt: ZonedDateTime, endsAt: ZonedDateTime): DateTimeInterval =
    DateTimeInterval(
      start = startsAt.toLocalDateTime,
      end = endsAt.toLocalDateTime
    )

  def planningEntry(rubyWeekDay: Int, startLocalTime: String, endLocalTime: String): TimeIntervalForWeekDay =
    planningEntry(rubyWeekDay, LocalTime.parse(startLocalTime), LocalTime.parse(endLocalTime))

  def planningEntry(rubyWeekDay: Int, startTime: LocalTime, endTime: LocalTime): TimeIntervalForWeekDay =
    TimeIntervalForWeekDay(
      dayOfWeek = rubyWeekDayToJavaWeekDay(rubyWeekDay),
      interval = TimeInterval(start = startTime, end = endTime)
    )

  def addInterval(intervals: Array[TimeInterval], startLocalTime: String, endLocalTime: String): List[TimeInterval] = {
    TimeInterval.mergeIntervals(
      intervals.toList ::: List(
        TimeInterval(start = LocalTime.parse(startLocalTime), end = LocalTime.parse(endLocalTime))
      )
    )
  }

  def schedule(
      plannings: Array[TimeIntervalForWeekDay],
      exceptions: Array[DateTimeInterval],
      timeZone: String
  ): JRubyzSchedule =
    new JRubyzSchedule(
      Schedule(planning = plannings.toList, exceptions = exceptions.toList, timeZone = ZoneId.of(timeZone))
    )

  private[jruby] def rubyWeekDayToJavaWeekDay(rubyWeekDay: Int): DayOfWeek =
    if (rubyWeekDay == 0) SUNDAY else DayOfWeek.of(rubyWeekDay)
}
