package com.colisweb.jrubysnesshours.core

import java.time.{LocalTime, LocalDateTime}
import scala.math.Ordering.Implicits._

final case class CutOff(limit: LocalTime, firstAvailableTime: LocalTime)

final case class DoubleCutOff(sameDay: CutOff, nextDay: CutOff) {
  def nextAvailableMoment(requestMoment: LocalDateTime): LocalDateTime = {
    val requestTime = requestMoment.toLocalTime
    if (requestTime <= sameDay.limit)
      requestMoment
    else if (requestTime <= nextDay.limit)
      LocalDateTime.of(requestMoment.toLocalDate, sameDay.firstAvailableTime)
    else
      LocalDateTime.of(requestMoment.toLocalDate.plusDays(1), nextDay.firstAvailableTime)
  }

}
