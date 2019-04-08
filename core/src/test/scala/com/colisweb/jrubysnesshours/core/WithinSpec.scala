package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.{Duration, LocalTime, ZoneId, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval
}
import org.scalatest.{Matchers, WordSpec}

class WithinSpec extends WordSpec with Matchers {

  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)
  }

  val planning: BusinessHoursByDayOfWeek = Map(
    MONDAY -> List(Interval("09:00".toLocalTime, "19:00".toLocalTime)),
    TUESDAY -> List(
      Interval("09:30".toLocalTime, "14:00".toLocalTime),
      Interval("15:00".toLocalTime, "19:00".toLocalTime)
    ),
    WEDNESDAY -> List(Interval("09:30".toLocalTime, "20:00".toLocalTime)),
    THURSDAY -> List(Interval("09:30".toLocalTime, "19:00".toLocalTime)),
    FRIDAY -> List(Interval("09:30".toLocalTime, "19:00".toLocalTime)),
    SATURDAY -> List(
      Interval("09:00".toLocalTime, "14:00".toLocalTime),
      Interval("15:00".toLocalTime, "19:00".toLocalTime)
    )
  )
  val zoneId: ZoneId = ZoneId.of("Europe/Paris")

  "within" should {

    val within: (ZonedDateTime, ZonedDateTime) => Duration =
      Within.within(planning, zoneId)

    "compute correct duration Thursday 18:00 to Friday 10:00" in {
      val d1 =
        within(aDayAt("2019-03-21", "18:00"), aDayAt("2019-03-22", "10:00"))
      d1 shouldBe aDuration(1, 30)
    }
    "compute correct duration Saturday 13:00 to same Saturday 16:00" in {
      val d2 =
        within(aDayAt("2019-03-23", "13:00"), aDayAt("2019-03-23", "16:00"))
      d2 shouldBe aDuration(2)
    }
    "compute correct duration Saturday 13:00 to Monday 10:00" in {
      val d3 =
        within(aDayAt("2019-03-23", "13:00"), aDayAt("2019-03-25", "10:00"))
      d3 shouldBe aDuration(6)
    }
    "compute correct duration Saturday 13:00 to Tuesday 16:00" in {
      val d4 =
        within(aDayAt("2019-03-23", "13:00"), aDayAt("2019-03-26", "16:00"))
      d4 shouldBe aDuration(20, 30)
    }
    "compute correct duration Sunday 13:00 to Tuesday 10:00" in {
      val d5 =
        within(aDayAt("2019-03-24", "13:00"), aDayAt("2019-03-26", "10:00"))
      d5 shouldBe aDuration(10, 30)
    }
    "compute correct duration Monday 09:00 to Sunday 23:00" in {
      val d6 =
        within(aDayAt("2019-03-18", "09:00"), aDayAt("2019-03-24", "23:00"))
      d6 shouldBe aDuration(2, 9, 0)
    }
  }

  def aDayAt(day: String, time: String): ZonedDateTime =
    ZonedDateTime.parse(s"${day}T$time:00.000+01:00[$zoneId]")

  def aDuration(hours: Int, minutes: Int = 0): Duration =
    Duration.ofHours(hours.toLong).plusMinutes(minutes.toLong)

  def aDuration(days: Int, hours: Int, minutes: Int): Duration =
    Duration
      .ofDays(days.toLong)
      .plusHours(hours.toLong)
      .plusMinutes(minutes.toLong)
}
