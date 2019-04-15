package com.colisweb.jrubysnesshours.core

import java.time.{DateTimeException, DayOfWeek, LocalDate, LocalDateTime, LocalTime, ZoneOffset, Duration => JDuration}
import java.util.concurrent.TimeUnit

import org.threeten.extra.Interval

import scala.collection.mutable.ListBuffer
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

  import TimeInterval._

  @inline private[this] def toInstant(localTime: LocalTime) = LocalDateTime.of(`1970-01-01`, localTime).toInstant(utc)
  private lazy val _interval                                = Interval.of(toInstant(start), toInstant(end))

  def isBefore(that: TimeInterval): Boolean    = this._interval isBefore that._interval
  def encloses(that: TimeInterval): Boolean    = this._interval encloses that._interval
  def isConnected(that: TimeInterval): Boolean = this._interval isConnected that._interval

  @inline def overlapOnlyOnTheStart(that: TimeInterval): Boolean =
    (this startsBefore that.start) && (this endsBefore that.end)

  /**
    * Non commutative substraction: x - y != y - x
    *
    * The passed interval will be substracted from the current interval.
    */
  def minus(that: TimeInterval): List[TimeInterval] =
    if (that._interval encloses this._interval) List.empty
    else if (this._interval encloses that._interval) {
      val acc = ListBuffer.empty[TimeInterval]
      if (this.start != that.start) acc += TimeInterval(start = this.start, end = that.start)
      if (this.end != that.end) acc += TimeInterval(start = that.end, end = this.end)
      acc.toList
    } else if (!(this._interval isConnected that._interval)) this :: Nil
    else if (this overlapOnlyOnTheStart that) TimeInterval(start = this.start, end = that.start) :: Nil
    else TimeInterval(that.end, this.end) :: Nil // overlapOnlyOnTheEnd

  def contains(time: LocalTime): Boolean             = this._interval.contains(toInstant(time))
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
  private[core] final val utc: ZoneOffset         = ZoneOffset.UTC
  private[core] final val `1970-01-01`: LocalDate = LocalDate.of(1970, 1, 1)
  private[core] final val END_OF_DAY: LocalTime   = LocalTime.of(23, 59, 0)
}
