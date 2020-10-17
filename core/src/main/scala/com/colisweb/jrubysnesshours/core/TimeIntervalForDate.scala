package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, ZoneId}

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  val start: LocalTime = interval.start
  val end: LocalTime   = interval.end

  def cutStart: Option[TimeInterval] =
    if (start == MIN) None else Some(TimeInterval(MIN, start))

  def cutEnd: Option[TimeInterval] =
    if (end == MAX) None else Some(TimeInterval(end, MAX))

  def cutBoth: List[TimeInterval] = cutStart.toList ++ cutEnd.toList

  def split(duration: Duration): List[TimeIntervalForDate] = interval.split(duration).map(i => copy(interval = i))

  private def changeTimeTimeZone(start: LocalDateTime, originTimeZone: ZoneId, targetTimeZone: ZoneId): LocalDateTime =
    start.atZone(originTimeZone).withZoneSameInstant(targetTimeZone).toLocalDateTime

  /** Return a new TimeIntervalForDate, corresponding to current interval in targetTimeZone
    * If converted interval straddle 2 days, return None (cannot be converted to unique TimeIntervalForDate)
    */
  def changeIntervalTimeZone(
      originTimeZone: ZoneId,
      targetTimeZone: ZoneId
  ): Option[TimeIntervalForDate] = {
    val startInTargetTimeZone = changeTimeTimeZone(LocalDateTime.of(date, start), originTimeZone, targetTimeZone)
    val endInTargetTimeZone   = changeTimeTimeZone(LocalDateTime.of(date, end), originTimeZone, targetTimeZone)
    if (startInTargetTimeZone.toLocalDate.equals(endInTargetTimeZone.toLocalDate)) {
      Some(
        TimeIntervalForDate(
          startInTargetTimeZone.toLocalDate,
          TimeInterval(startInTargetTimeZone.toLocalTime, endInTargetTimeZone.toLocalTime)
        )
      )
    } else {
      None
    }
  }
}
