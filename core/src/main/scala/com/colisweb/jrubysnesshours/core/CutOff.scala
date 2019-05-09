package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalDateTime, LocalTime}

import scala.math.Ordering.Implicits._

final case class CutOff(limit: LocalTime, firstAvailableTime: LocalTime)

final case class DoubleCutOff(sameDay: CutOff, nextDay: CutOff) {
  def nextAvailableMoment(requestTime: LocalTime): AvailableFrom = {
    if (requestTime <= sameDay.limit)
      AvailableFrom(availableTime = requestTime)
    else if (requestTime <= nextDay.limit)
      AvailableFrom(availableTime = sameDay.firstAvailableTime)
    else
      AvailableFrom(availableTime = nextDay.firstAvailableTime, sameDay = false)
  }
}

final case class AvailableFrom(availableTime: LocalTime, sameDay: Boolean = true) {
  def forDate(date: LocalDate): LocalDateTime =
    if (sameDay) date.atTime(availableTime)
    else date.plusDays(1).atTime(availableTime)
}
