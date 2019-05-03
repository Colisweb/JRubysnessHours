package com.colisweb.jrubysnesshours.core

import java.time._
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

  def isBefore(that: TimeInterval): Boolean = this.end <= that.start

  def isAfter(that: TimeInterval): Boolean = this.start >= that.end

  def encloses(that: TimeInterval): Boolean =
    this.start <= that.start && that.end <= this.end

  def isConnected(that: TimeInterval): Boolean =
    this.start <= that.end && that.start <= this.end

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

  def contains(time: LocalTime): Boolean = start <= time && time < end

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
