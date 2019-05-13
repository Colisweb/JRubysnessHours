package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalDateTime, LocalTime}

import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.math.Ordering.Implicits._

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  val start: LocalTime = interval.start
  val end: LocalTime   = interval.end

  def roundToFullHours: Iterable[TimeIntervalForDate] = interval.roundToFullHours.map(i => copy(interval = i))

  def split(hours: Long): List[TimeIntervalForDate] = interval.split(hours).map(i => copy(interval = i))
}

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime) {
  assert(start < end, s"DateTimeInterval error: 'start' ($start) is after 'end' ($end)")

  def asSameDateInterval: Option[TimeIntervalForDate] =
    if (start.toLocalDate == end.toLocalDate)
      Some(TimeIntervalForDate(start.toLocalDate, TimeInterval(start.toLocalTime, end.toLocalTime)))
    else
      None
}
