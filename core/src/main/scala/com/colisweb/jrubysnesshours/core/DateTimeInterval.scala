package com.colisweb.jrubysnesshours.core

import java.time.LocalDateTime
import java.time.LocalTime.{MAX, MIN}
import java.time.temporal.ChronoUnit.DAYS

import com.colisweb.jrubysnesshours.core.TimeInterval.{FULL_DAY, toEndOfDay}
import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.math.Ordering.Implicits._

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime) {
  assert(start < end, s"DateTimeInterval error: 'start' ($start) is after 'end' ($end)")

  def asSameDateIntervals: List[TimeIntervalForDate] = {
    val nextDayStart = start.plusDays(1L).truncatedTo(DAYS)
    if (nextDayStart < end)
      TimeIntervalForDate(start.toLocalDate, toEndOfDay(start.toLocalTime)) ::
        copy(start = nextDayStart).asSameDateIntervals
    else List(asTimeIntervalForDate)
  }

  def asTimeIntervalForDate: TimeIntervalForDate =
    TimeIntervalForDate(start.toLocalDate, TimeInterval(start.toLocalTime, end.toLocalTime))

  def toTimeInterval: TimeInterval = TimeInterval(start.toLocalTime, end.toLocalTime)

  def days: Long = start.toLocalDate.until(end.toLocalDate, DAYS)

  def splitByDates: Iterable[TimeIntervalForDate] = {
    val localStartTime = start.toLocalTime
    val localEndTime   = end.toLocalTime

    val localStartDate = start.toLocalDate
    val localEndDate   = end.toLocalDate

    if (days == 0L)
      List(TimeIntervalForDate(date = localStartDate, interval = toTimeInterval))
    else {
      val midDays = (1L until days)
        .map(i => TimeIntervalForDate(date = localStartDate.plusDays(i), interval = FULL_DAY))

      val firstDay =
        if (localStartTime < MAX) {
          Some(
            TimeIntervalForDate(
              date = localStartDate,
              interval = TimeInterval(start = localStartTime, end = MAX)
            )
          )
        } else None

      val lastDay =
        if (MIN < localEndTime)
          Some(
            TimeIntervalForDate(
              date = localEndDate,
              interval = TimeInterval(start = MIN, end = localEndTime)
            )
          )
        else None

      firstDay ++ midDays ++ lastDay
    }
  }
}
