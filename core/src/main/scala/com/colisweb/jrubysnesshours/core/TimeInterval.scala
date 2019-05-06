package com.colisweb.jrubysnesshours.core

import java.time._
import java.time.temporal.ChronoUnit

import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.math.Ordering.Implicits._

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime) {

  assert(start < end, s"DateTimeInterval error: 'start' ($start) is after 'end' ($end)")
}

final case class TimeIntervalForWeekDay(dayOfWeek: DayOfWeek, interval: TimeInterval)

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  val start: LocalTime = interval.start
  val end: LocalTime   = interval.end
}

final case class TimeInterval(start: LocalTime, end: LocalTime) {

  assert(start < end, s"TimeInterval error: 'start' ($start) must be < 'end' ($end)")

  def roundToFullHours: Option[TimeInterval] = {
    val roundedStart = start.plusHours(if (start.getMinute > 0) 1 else 0).withMinute(0)
    val roundedEnd   = end.withMinute(0)
    if (roundedEnd > roundedStart)
      Some(TimeInterval(roundedStart, roundedEnd))
    else None
  }

  def split(hours: Long): List[TimeInterval] =
    if (start.until(end, ChronoUnit.HOURS) == hours)
      List(this)
    else if (start.until(end, ChronoUnit.HOURS) < hours)
      Nil
    else TimeInterval(start, start.plusHours(hours)) :: copy(start = start.plusHours(hours)).split(hours)

  def isBefore(that: TimeInterval): Boolean = this.end <= that.start

  def isAfter(that: TimeInterval): Boolean = this.start >= that.end

  def encloses(that: TimeInterval): Boolean =
    this.start <= that.start && that.end <= this.end

  def enclosesStrict(that: TimeInterval): Boolean =
    this.start < that.start && that.end < this.end

  def isConnected(that: TimeInterval): Boolean =
    this.start <= that.end && that.start <= this.end

  def diff(that: TimeInterval): List[TimeInterval] = {
    if (that encloses this) Nil
    else if (this enclosesStrict that) copy(end = that.start) :: copy(start = that.end) :: Nil
    else if (that.contains2(this.end)) copy(end = that.start) :: Nil
    else if (that.contains(this.start)) copy(start = that.end) :: Nil
    else this :: Nil
  }

  def union(that: TimeInterval): TimeInterval = {
    assert(isConnected(that), s"Intervals do not connect: $this and $that")
    if (that encloses this) that
    else if (this encloses that) this
    else TimeInterval(start = start min that.start, end = end max that.end)
  }

  def contains(time: LocalTime): Boolean = start <= time && time < end

  def contains2(time: LocalTime): Boolean = start < time && time <= end

}
