package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.{Duration, LocalDate, LocalTime, ZoneId, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval,
  TimeSegment
}

object SpecUtils {
  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)

    def -(to: String): Interval = Interval(str.toLocalTime, to.toLocalTime)
  }

  val zoneId: ZoneId = ZoneId.of("Europe/Paris")

  def aDayAt(day: String, time: String): ZonedDateTime =
    ZonedDateTime.parse(s"${day}T$time:00.000+01:00[$zoneId]")

  def aSegment(date: String, startTime: String, endTime: String) =
    TimeSegment(
      LocalDate.parse(date),
      Interval(
        LocalTime.parse(s"$startTime:00"),
        LocalTime.parse(s"$endTime:00")
      )
    )

  def aDuration(hours: Int, minutes: Int = 0): Duration =
    Duration.ofHours(hours.toLong).plusMinutes(minutes.toLong)

  def aDuration(days: Int, hours: Int, minutes: Int): Duration =
    Duration
      .ofDays(days.toLong)
      .plusHours(hours.toLong)
      .plusMinutes(minutes.toLong)

  val planning: BusinessHoursByDayOfWeek = Map(
    MONDAY -> List("09:00" - "19:00"),
    TUESDAY -> List("09:30" - "14:00", "15:00" - "19:00"),
    WEDNESDAY -> List("09:30" - "20:00"),
    THURSDAY -> List("09:30" - "19:00"),
    FRIDAY -> List("09:30" - "19:00"),
    SATURDAY -> List("09:00" - "14:00", "15:00" - "19:00")
  )
}
