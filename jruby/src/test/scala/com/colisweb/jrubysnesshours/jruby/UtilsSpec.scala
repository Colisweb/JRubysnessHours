package com.colisweb.jrubysnesshours.jruby

import java.time.{LocalDate, LocalTime}

import com.colisweb.jrubysnesshours.core.TimeInterval

object UtilsSpec {

  def parseDate(date: String): LocalDate = LocalDate.parse(date)

  def parseInterval(startTime: String, endTime: String) =
    TimeInterval(
      LocalTime.parse(s"$startTime:00"),
      LocalTime.parse(s"$endTime:00")
    )

  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)
  }
}
