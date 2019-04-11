package com.colisweb.jrubysnesshours.jruby

import java.time.{DayOfWeek, LocalTime, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, TimeInterval, TimeIntervalForWeekDay}

object JRuby {

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

  private[jruby] def rubyWeekDayToJavaWeekDay(rubyWeekDay: Int): DayOfWeek = rubyWeekDay match {
    case 0 => DayOfWeek.SUNDAY
    case _ => DayOfWeek.of(rubyWeekDay)
  }

}
