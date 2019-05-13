package com.colisweb.jrubysnesshours.jruby

import java.time.{LocalDate, ZoneId, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.TimeIntervalForDate
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule.UTC

object RubyTimeSegmentInterval {
  def apply(timeIntervalForDate: TimeIntervalForDate, timeZone: ZoneId): RubyTimeSegmentInterval = {
    val start: ZonedDateTime =
      ZonedDateTime
        .of(timeIntervalForDate.date, timeIntervalForDate.start, timeZone)
        .withZoneSameInstant(UTC)

    val end =
      ZonedDateTime
        .of(timeIntervalForDate.date, timeIntervalForDate.end, timeZone)
        .withZoneSameInstant(UTC)

    RubyTimeSegmentInterval(
      date = timeIntervalForDate.date,
      startTime = start,
      endTime = end
    )
  }

  def apply(localDate: String, startZonedDateTime: String, endZonedDateTime: String): RubyTimeSegmentInterval =
    RubyTimeSegmentInterval(
      date = LocalDate.parse(localDate),
      startTime = ZonedDateTime.parse(startZonedDateTime),
      endTime = ZonedDateTime.parse(endZonedDateTime)
    )
}

final case class RubyTimeSegmentInterval(date: LocalDate, startTime: ZonedDateTime, endTime: ZonedDateTime)
