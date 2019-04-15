package com.colisweb.jrubysnesshours.jruby

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalTime, ZoneId, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, Schedule, TimeInterval, TimeIntervalForWeekDay}
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule._

import scala.concurrent.duration._

final class JRubyzSchedule private[jruby] (schedule: Schedule) {

  def timeSegments(startsAt: String, endsAt: String): Array[RubyTimeSegmentInterval] =
    schedule
      .intervalsBetween(ZonedDateTime.parse(startsAt), ZonedDateTime.parse(endsAt))
      .map { timeIntervalForDate =>
        val start = timeIntervalForDate.date
          .atTime(timeIntervalForDate.start)
          .atZone(schedule.timeZone)
          .withZoneSameInstant(UTC)
        val end = timeIntervalForDate.date
          .atTime(timeIntervalForDate.end)
          .atZone(schedule.timeZone)
          .withZoneSameInstant(UTC)

        RubyTimeSegmentInterval(
          timeIntervalForDate.date.format(ISO_DATE_FORMATTER),
          start.format(ISO_8601_FORMATTER),
          end.format(ISO_8601_FORMATTER)
        )
      }
      .toArray

  def within(start: String, end: String): Duration = {
    schedule.within(ZonedDateTime.parse(start), ZonedDateTime.parse(end))
  }
}

object JRubyzSchedule {

  private val UTC                = ZoneId.of("UTC")
  private val ISO_DATE_FORMATTER = DateTimeFormatter.ISO_DATE
  private val ISO_8601_FORMATTER = DateTimeFormatter.ISO_DATE_TIME

  final case class RubyTimeSegmentInterval(date: String, startTime: String, endTime: String)

  def rubyToDateTimeInterval(startsAt: String, endsAt: String): DateTimeInterval = {
    DateTimeInterval(
      start = ZonedDateTime.parse(startsAt).toLocalDateTime,
      end = ZonedDateTime.parse(endsAt).toLocalDateTime
    )
  }

  def rubyToPlanning(rubyWeekDay: Int, startTime: String, endTime: String): TimeIntervalForWeekDay = {
    TimeIntervalForWeekDay(
      dayOfWeek = rubyWeekDayToJavaWeekDay(rubyWeekDay),
      interval = TimeInterval(start = LocalTime.parse(startTime), end = LocalTime.parse(endTime))
    )
  }

  def schedule(
      plannings: Array[TimeIntervalForWeekDay],
      exceptions: Array[DateTimeInterval],
      timeZone: String
  ): JRubyzSchedule = new JRubyzSchedule(Schedule(plannings.toList, exceptions.toList, stringToZoneId(timeZone)))

  private[jruby] def stringToZoneId(strZoneId: String): ZoneId = ZoneId.of(strZoneId)

  private[jruby] def rubyWeekDayToJavaWeekDay(rubyWeekDay: Int): DayOfWeek = rubyWeekDay match {
    case 0 => DayOfWeek.SUNDAY
    case _ => DayOfWeek.of(rubyWeekDay)
  }
}
