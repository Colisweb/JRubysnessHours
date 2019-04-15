package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalTime}

object UtilsSpec {

  def parseDate(date: String): LocalDate = LocalDate.parse(date)

  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)
  }
}
