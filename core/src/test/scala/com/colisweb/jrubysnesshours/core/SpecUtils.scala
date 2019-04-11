package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.{Duration, LocalDate, LocalTime, ZoneId, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval,
  TimeSegment
}

object SpecUtils {

  implicit class StringToLocalTimeOps(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)

    def -(to: String): Interval = Interval(str.toLocalTime, to.toLocalTime)

    def at(interval: Interval): TimeSegment =
      TimeSegment(LocalDate.parse(str), interval)

    def at(time: String): ZonedDateTime =
      ZonedDateTime.parse(s"${str}T$time:00.000+01:00[$zoneId]")
  }

  implicit class LocalDateOps(localDate: LocalDate) {
    def ts(startTime: LocalTime, endTime: LocalTime): TimeSegment = {
      val t = TimeSegment(localDate, Interval(startTime, endTime))
      println(t) //TODO: remove when enough tests are "tested"
      t
    }
  }

  val planning: BusinessHoursByDayOfWeek = Map(
    MONDAY -> List("09:00" - "19:00"),
    TUESDAY -> List("09:30" - "14:00", "15:00" - "19:00"),
    WEDNESDAY -> List("09:30" - "20:00"),
    THURSDAY -> List("09:30" - "19:00"),
    FRIDAY -> List("09:30" - "19:00"),
    SATURDAY -> List("09:00" - "14:00", "15:00" - "19:00")
  )

  val zoneId: ZoneId = ZoneId.of("Europe/Paris")

  val exceptions: List[TimeSegment] = List("2019-04-08" at "13:00" - "16:00")

  def aDayAt(day: String, time: String): ZonedDateTime =
    day at time

  def aDuration(hours: Int, minutes: Int = 0): Duration =
    Duration.ofHours(hours.toLong).plusMinutes(minutes.toLong)

  def aDuration(days: Int, hours: Int, minutes: Int): Duration =
    Duration
      .ofDays(days.toLong)
      .plusHours(hours.toLong)
      .plusMinutes(minutes.toLong)

}
