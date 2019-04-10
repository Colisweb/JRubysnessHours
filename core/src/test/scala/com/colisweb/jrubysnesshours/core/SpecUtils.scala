package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.LocalTime

import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval
}

object SpecUtils {
  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)

    def -(to: String): Interval = Interval(str.toLocalTime, to.toLocalTime)
  }

  val planning: BusinessHoursByDayOfWeek = Map(
    MONDAY -> List("09:00" - "19:00"),
    TUESDAY -> List("09:30" - "14:00", "15:00" - "19:00"),
    WEDNESDAY -> List("09:30" - "20:00"),
    THURSDAY -> List("09:30" - "19:00"),
    FRIDAY -> List("09:30" - "19:00"),
    SATURDAY -> List("09:00" - "14:00", "15:00" - "19:00")
  )
}
