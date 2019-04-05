package com.colisweb.jrubysnesshours.core

import java.time.{DayOfWeek, LocalTime}

object Core {

  case class Interval(startTime: LocalTime, endTime: LocalTime)
  case class BusinessHour(dayOfWeek: DayOfWeek, interval: Interval)
  case class BusinessHoursForDayOfWeek(dayOfWeek: DayOfWeek, intervals: List[Interval])

}
