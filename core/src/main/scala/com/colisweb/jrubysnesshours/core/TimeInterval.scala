package com.colisweb.jrubysnesshours.core

import java.time.{DayOfWeek, LocalDate, LocalTime}
import scala.math.Ordering.Implicits._

case class TimeInterval(start: LocalTime, end: LocalTime)

case class TimeIntervalForWeekDay(dayOfWeek: DayOfWeek, interval: TimeInterval)

case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {

  def startTime: LocalTime = interval.start
  def endTime: LocalTime   = interval.end
}

object TimeInterval {

  implicit class TimeIntervalOps(interval: TimeInterval) {

    def contains(instant: LocalTime): Boolean =
      interval.start >= instant && instant <= interval.end

    def containsNot(instant: LocalTime): Boolean =
      interval.start < instant || instant > interval.end
  }
}
