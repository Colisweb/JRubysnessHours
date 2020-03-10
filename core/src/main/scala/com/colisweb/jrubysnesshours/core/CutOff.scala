package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalDateTime, LocalTime}

import scala.math.Ordering.Implicits._

final case class CutOff(limit: LocalTime, firstAvailableTime: LocalTime)

final case class DoubleCutOff(sameDay: CutOff, nextDay: CutOff) {
  assert(sameDay.limit <= nextDay.limit, s"sameDay $sameDay limit must be <= to nextDay $nextDay limit")

  def nextAvailableMoment(requestTime: LocalTime, date: LocalDate, nextDate: LocalDate): LocalDateTime = {
    if (requestTime <= sameDay.limit)
      date.atTime(requestTime)
    else if (requestTime <= nextDay.limit)
      date.atTime(sameDay.firstAvailableTime)
    else
      nextDate.atTime(nextDay.firstAvailableTime)
  }
}
