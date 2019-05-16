package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time._
import java.time.temporal.ChronoUnit.HOURS

import scala.math.Ordering.Implicits._

final case class TimeInterval(start: LocalTime, end: LocalTime) {
  assert(start < end, s"TimeInterval error: 'start' ($start) must be < 'end' ($end)")

  def roundToFullHours: Option[TimeInterval] = {
    val ceilingStart = start.plusHours(if (start.getMinute + start.getSecond > 0) 1 else 0).truncatedTo(HOURS)
    val floorEnd     = end.truncatedTo(HOURS)
    if (floorEnd > ceilingStart)
      Some(TimeInterval(ceilingStart, floorEnd))
    else None
  }

  def split(hours: Long): List[TimeInterval] =
    if (start.until(end, HOURS) == hours)
      List(this)
    else if (start.until(end, HOURS) < hours)
      Nil
    else TimeInterval(start, start.plusHours(hours)) :: copy(start = start.plusHours(1)).split(hours)

  def isBefore(that: TimeInterval): Boolean = this.end <= that.start

  def isAfter(that: TimeInterval): Boolean = this.start >= that.end

  def encloses(that: TimeInterval): Boolean =
    this.start <= that.start && that.end <= this.end

  def enclosesStrict(that: TimeInterval): Boolean =
    this.start < that.start && that.end < this.end

  def isConnected(that: TimeInterval): Boolean =
    this.start <= that.end && that.start <= this.end

  def isConnectedStrict(that: TimeInterval): Boolean =
    this.start < that.end && that.start < this.end

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

  def merge(that: TimeInterval): List[TimeInterval] =
    if (this isBefore that) List(this, that)
    else if (this encloses that) List(this)
    else List(this union that)

  def contains(time: LocalTime): Boolean = start <= time && time < end

  def contains2(time: LocalTime): Boolean = start < time && time <= end
}

object TimeInterval {
  val FULL_DAY = TimeInterval(MIN, MAX)

  def cutStartOfDay(start: LocalTime): Option[TimeInterval] =
    if (start == MIN) None
    else Some(fromStartOfDay(start))

  def fromStartOfDay(end: LocalTime): TimeInterval =
    TimeInterval(start = MIN, end = end)

  def toEndOfDay(start: LocalTime) =
    TimeInterval(start = start, end = MAX)

  // TODO: can be simplified in scala 2.13
  // https://github.com/scala/scala/blob/v2.13.0-M5/src/library/scala/collection/Iterable.scala#L578
  def toMap[T, K](intervals: List[T])(fKey: T => K, fInterval: T => TimeInterval): Map[K, List[TimeInterval]] =
    intervals
      .groupBy(fKey)
      .mapValues(l => mergeIntervals(l.map(fInterval)))

  def mergeIntervals(intervals: List[TimeInterval]): List[TimeInterval] =
    intervals
      .sortBy(_.start)
      .foldRight(List.empty[TimeInterval]) {
        case (interval, h :: t) => (interval merge h) ::: t // TODO: `:::` is not in constant time.
        case (interval, Nil)    => List(interval)
      }
}
