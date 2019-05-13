package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time.temporal.ChronoUnit.DAYS
import java.time.{LocalDate, LocalDateTime, LocalTime}

import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.math.Ordering.Implicits._

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

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime) {
  assert(start < end, s"DateTimeInterval error: 'start' ($start) is after 'end' ($end)")

  def asSameDateIntervals: List[TimeIntervalForDate] = {
    val nextDayStart = start.plusDays(1L).truncatedTo(DAYS)
    if (nextDayStart < end)
      TimeIntervalForDate(start.toLocalDate, TimeInterval(start.toLocalTime, MAX)) ::
        copy(start = nextDayStart).asSameDateIntervals
    else List(asTimeIntervalForDate)
  }

  def asTimeIntervalForDate: TimeIntervalForDate =
    TimeIntervalForDate(start.toLocalDate, TimeInterval(start.toLocalTime, end.toLocalTime))

}
