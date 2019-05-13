package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalTime}

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  val start: LocalTime = interval.start
  val end: LocalTime   = interval.end

  def roundToFullHours: Iterable[TimeIntervalForDate] = interval.roundToFullHours.map(i => copy(interval = i))

  def split(hours: Long): List[TimeIntervalForDate] = interval.split(hours).map(i => copy(interval = i))
}
