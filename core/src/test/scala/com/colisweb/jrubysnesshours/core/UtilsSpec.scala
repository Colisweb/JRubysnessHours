package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalTime}

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
