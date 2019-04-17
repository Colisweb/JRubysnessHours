package com.colisweb.jrubysnesshours.core

import java.time._

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime) {
  assert(start isBefore end, s"DateTimeInterval error: 'start' ($start) is after 'end' ($end)")
}

final case class TimeIntervalForWeekDay(dayOfWeek: DayOfWeek, interval: TimeInterval)

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  val start: LocalTime = interval.start
  val end: LocalTime   = interval.end
}

final case class TimeInterval(start: LocalTime, end: LocalTime) {
  assert(start isBefore end, s"TimeInterval error: 'start' ($start) is after 'end' ($end)")

  /**
    * Copied from `org.threeten.extra.Interval`.
    */
  def isBefore(that: TimeInterval): Boolean = this.end.compareTo(that.start) <= 0

  /**
    * Copied from `org.threeten.extra.Interval`.
    *
    * But improved thanks to boolean logic.
    */
  def encloses(that: TimeInterval): Boolean =
    !(this.start.compareTo(that.start) > 0 || that.end.compareTo(this.end) > 0)

  /**
    * Copied from `org.threeten.extra.Interval`.
    *
    * But improved thanks to boolean logic.
    */
  def isConnected(that: TimeInterval): Boolean =
    !(this.start.compareTo(that.end) > 0 || that.start.compareTo(this.end) > 0)

  /**
    * Non commutative substraction: x - y != y - x
    *
    * The passed interval will be substracted from the current interval.
    */
  def minus(that: TimeInterval): List[TimeInterval] = {
    val cmpThisStartToThatEnd      = this.start.compareTo(that.end)
    lazy val cmpThisEndToThatStart = this.end.compareTo(that.start)
    lazy val cmpStart              = this.start.compareTo(that.start)
    lazy val cmpEnd                = that.end.compareTo(this.end)

    @inline def areNotConnected      = cmpThisStartToThatEnd > 0 || cmpThisEndToThatStart <= 0
    @inline def thatEnclosesThis     = !(cmpStart < 0 || cmpEnd < 0)
    @inline def thisEnclosesThat     = !(cmpStart >= 0 || cmpEnd >= 0)
    @inline def thatOverlapThisEnd   = !(cmpStart >= 0 || cmpThisEndToThatStart <= 0 || cmpEnd < 0)
    @inline def thatOverlapThisStart = !(cmpStart < 0 || cmpThisStartToThatEnd >= 0 || cmpEnd >= 0)

    if (areNotConnected) this :: Nil
    else if (thatEnclosesThis) Nil
    else if (thisEnclosesThat)
      TimeInterval(start = this.start, end = that.start) :: TimeInterval(start = that.end, end = this.end) :: Nil
    else if (thatOverlapThisEnd) TimeInterval(start = this.start, end = that.start) :: Nil
    else if (thatOverlapThisStart) TimeInterval(start = that.end, end = this.end) :: Nil
    else this :: Nil
  }

  /**
    * Copied from `org.threeten.extra.Interval`.
    *
    * But improved thanks to boolean logic.
    */
  def contains(time: LocalTime): Boolean = !(start.compareTo(time) > 0 || time.compareTo(end) >= 0)

  /**
    * Copied from `org.threeten.extra.Interval`.
    *
    * But improved thanks to boolean logic.
    */
  def union(that: TimeInterval): TimeInterval = {
    if (!isConnected(that)) throw new DateTimeException(s"Intervals do not connect: $this and $that")

    val cmpStart = start.compareTo(that.start)
    val cmpEnd   = end.compareTo(that.end)

    if (!(cmpStart < 0 || cmpEnd > 0)) that
    else if (!(cmpStart > 0 || cmpEnd < 0)) this
    else {
      val newStart = if (cmpStart >= 0) that.start else start
      val newEnd   = if (cmpEnd <= 0) that.end else end
      TimeInterval(start = newStart, end = newEnd)
    }
  }
}
