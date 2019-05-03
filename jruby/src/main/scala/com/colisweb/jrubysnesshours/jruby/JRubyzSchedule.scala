package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._
import java.time._

import com.colisweb.jrubysnesshours.core._

final case class RubyTimeSegmentInterval(date: LocalDate, startTime: ZonedDateTime, endTime: ZonedDateTime)

final class JRubyzSchedule private[jruby] (schedule: Schedule) {

  import JRubyzSchedule._

  def timeSegments(startsAt: ZonedDateTime, endsAt: ZonedDateTime): Array[RubyTimeSegmentInterval] =
    schedule
      .intervalsBetween(startsAt, endsAt)
      .map { timeIntervalForDate =>
        val start: ZonedDateTime =
          ZonedDateTime
            .of(timeIntervalForDate.date, timeIntervalForDate.start, schedule.timeZone)
            .withZoneSameInstant(UTC)

        val end =
          ZonedDateTime
            .of(timeIntervalForDate.date, timeIntervalForDate.end, schedule.timeZone)
            .withZoneSameInstant(UTC)

        RubyTimeSegmentInterval(
          date = timeIntervalForDate.date,
          startTime = start,
          endTime = end
        )
      }
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

  private val UTC: ZoneId = ZoneId.of("UTC")

  def exception(startsAt: ZonedDateTime, endsAt: ZonedDateTime): DateTimeInterval =
    DateTimeInterval(
      start = startsAt.toLocalDateTime,
      end = endsAt.toLocalDateTime
    )

  def planningEntry(rubyWeekDay: Int, startTime: LocalTime, endTime: LocalTime): TimeIntervalForWeekDay =
    TimeIntervalForWeekDay(
      dayOfWeek = rubyWeekDayToJavaWeekDay(rubyWeekDay),
      interval = TimeInterval(start = startTime, end = endTime)
    )

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
