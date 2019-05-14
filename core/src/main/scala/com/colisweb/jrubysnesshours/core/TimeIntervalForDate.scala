package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time.{LocalDate, LocalTime}

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  val start: LocalTime = interval.start
  val end: LocalTime   = interval.end

  def cutStart: Option[TimeInterval] =
    if (start == MIN) None else Some(TimeInterval(MIN, start))

  def cutEnd: Option[TimeInterval] =
    if (end == MAX) None else Some(TimeInterval(end, MAX))

  def cutBoth: List[TimeInterval] = cutStart.toList ++ cutEnd.toList

  def roundToFullHours: Iterable[TimeIntervalForDate] = interval.roundToFullHours.map(i => copy(interval = i))

  def split(hours: Long): List[TimeIntervalForDate] = interval.split(hours).map(i => copy(interval = i))
}
