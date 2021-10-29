package com.colisweb.jrubysnesshours.core

import java.time.LocalTime.{MAX, MIN}
import java.time._
import java.time.temporal.ChronoUnit.HOURS
import scala.collection.compat._

import com.colisweb.jrubysnesshours.core.TimeInterval._

import scala.math.Ordering.Implicits._

// This class cannot represent an Interval ending at 00:00
final case class TimeInterval(start: LocalTime, end: LocalTime) {

  // import to use library scala.collection.compat even in scala 2.13
  implicit val collectionCompat = BuildFrom

  assert(start < end, s"TimeInterval error: 'start' ($start) must be < 'end' ($end)")

  private[core] def roundToMinutes(minutes: Long): Option[TimeInterval] = {
    val ceilingStart = validStart(minutes)
    val floorEnd     = validEnd(minutes)
    if (floorEnd > ceilingStart)
      Some(TimeInterval(ceilingStart, floorEnd))
    else None
  }

  // If range < 60 => round start to next rangeMinutes
  // if >= 60 => round to next hour
  private def validStart(rangeMinutes: Long): LocalTime =
    start.truncatedTo(HOURS).plusMinutes(ceiling(start.getMinute, rangeMinutes min 60L))

  // If range < 60 => round end to previous rangeMinutes
  // if = 60 => round to previous hour
  // if 90 => round to previous 30 min
  // if 135 (2h15) => round to previous 15 min
  private def validEnd(rangeMinutes: Long): LocalTime = {
    val rest = rangeMinutes % 60
    end.truncatedTo(HOURS).plusMinutes(floor(end.getMinute, if (rest == 0) 60 else rest))
  }

  def split(duration: Duration): List[TimeInterval] =
    splitMinutes(duration.toMinutes)

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

  private def splitMinutes(minutes: Long): List[TimeInterval] =
    roundToMinutes(minutes).toList.flatMap(_.splitRec(minutes))

  private def splitRec(minutes: Long): List[TimeInterval] = {
    val splitEnd = start plusMinutes minutes

    splitEnd.compareTo(end) match {
      case 0 => List(this)
      case x if x < 0 && start < splitEnd =>
        val split = TimeInterval(start, splitEnd)
        val shift = Duration.ofMinutes(minutes) min Duration.ofHours(1)
        split :: copy(start = start.plus(shift)).splitRec(minutes)
      case _ => Nil
    }
  }
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

  def toMap[T, K](intervals: List[T])(fKey: T => K, fInterval: T => TimeInterval): Map[K, List[TimeInterval]] = {
    intervals.groupMap(fKey)(fInterval).view.mapValues(mergeIntervals).toMap
  }

  def mergeIntervals(intervals: List[TimeInterval]): List[TimeInterval] =
    intervals
      .sortBy(_.start)
      .foldRight(List.empty[TimeInterval]) {
        case (interval, h :: t) => (interval merge h) ::: t // TODO: `:::` is not in constant time.
        case (interval, Nil)    => List(interval)
      }

  private def ceiling(number: Int, round: Long): Long = {
    round * (number / round + (if (number % round > 0) 1 else 0))
  }

  private def floor(number: Int, round: Long): Long = round * (number / round)
}
