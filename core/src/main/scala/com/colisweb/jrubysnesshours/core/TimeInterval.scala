package com.colisweb.jrubysnesshours.core

import java.time.{DateTimeException, DayOfWeek, LocalDate, LocalDateTime, LocalTime, Duration => JDuration}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime)

final case class TimeIntervalForWeekDay(dayOfWeek: DayOfWeek, interval: TimeInterval)

final case class TimeIntervalForDate(date: LocalDate, interval: TimeInterval) {
  @inline def start: LocalTime = interval.start
  @inline def end: LocalTime   = interval.end
  lazy val duration: Duration  = Duration(JDuration.between(start, end).getSeconds, TimeUnit.SECONDS)
}

final case class TimeInterval(start: LocalTime, end: LocalTime) {
  assert(start isBefore end)

  /**
    * Copied from `org.threeten.extra.Interval`.
    */
  def isBefore(that: TimeInterval): Boolean = this.end.compareTo(that.start) <= 0

  /**
    * Copied from `org.threeten.extra.Interval`.
    */
  def encloses(that: TimeInterval): Boolean = this.start.compareTo(that.start) <= 0 && that.end.compareTo(this.end) <= 0

  /**
    * Copied from `org.threeten.extra.Interval`.
    */
  def isConnected(that: TimeInterval): Boolean =
    this.start.compareTo(that.end) <= 0 && that.start.compareTo(this.end) <= 0

  /**
    * Non commutative substraction: x - y != y - x
    *
    * The passed interval will be substracted from the current interval.
    */
  def minus(that: TimeInterval): List[TimeInterval] = {
    val startsCompare = this.start.compareTo(that.start)
    val endsCompare   = that.end.compareTo(this.end)

    @inline def thisStartCompareToThatEnd = this.start.compareTo(that.end)
    @inline def thisEndCompareToThatStart = this.end.compareTo(that.start)

    @inline def thatEnclosesThis     = startsCompare >= 0 && endsCompare >= 0
    @inline def thisEnclosesThat     = startsCompare < 0 && endsCompare < 0
    @inline def areNotConnected      = thisStartCompareToThatEnd > 0 || thisEndCompareToThatStart <= 0
    @inline def thatOverlapThisEnd   = startsCompare < 0 && thisEndCompareToThatStart > 0 && endsCompare >= 0
    @inline def thatOverlapThisStart = startsCompare >= 0 && thisStartCompareToThatEnd < 0 && endsCompare < 0

    if (thatEnclosesThis) Nil
    else if (thisEnclosesThat)
      TimeInterval(start = this.start, end = that.start) :: TimeInterval(start = that.end, end = this.end) :: Nil
    else if (areNotConnected) this :: Nil
    else if (thatOverlapThisEnd) TimeInterval(start = this.start, end = that.start) :: Nil
    else if (thatOverlapThisStart) TimeInterval(start = that.end, end = this.end) :: Nil
    else this :: Nil
  }

  /**
    * Copied from `org.threeten.extra.Interval`.
    */
  def contains(time: LocalTime): Boolean             = start.compareTo(time) <= 0 && time.compareTo(end) < 0
  @inline def endsBefore(time: LocalTime): Boolean   = this.end isBefore time
  @inline def endsAfter(time: LocalTime): Boolean    = this.end isAfter time
  @inline def startsBefore(time: LocalTime): Boolean = this.start isBefore time
  @inline def startsAfter(time: LocalTime): Boolean  = this.start isAfter time

  /**
    * Copied from `org.threeten.extra.Interval`.
    *
    * Here, it's not possible to directly use `this._interval union that._interval` because we're unable to
    * then convert the result of that call to a `TimeInterval` because we can't easily convert an `Instant` to
    * a `LocalTime`.
    */
  def union(that: TimeInterval): TimeInterval = {
    if (!isConnected(that)) throw new DateTimeException(s"Intervals do not connect: $this and $that")

    val cmpStart = start.compareTo(that.start)
    val cmpEnd   = end.compareTo(that.end)

    if (cmpStart >= 0 && cmpEnd <= 0) that
    else if (cmpStart <= 0 && cmpEnd >= 0) this
    else {
      val newStart = if (cmpStart >= 0) that.start else start
      val newEnd   = if (cmpEnd <= 0) that.end else end
      TimeInterval(start = newStart, end = newEnd)
    }
  }
}

object TimeInterval {
  private[core] final val END_OF_DAY: LocalTime = LocalTime.of(23, 59, 0)
}
