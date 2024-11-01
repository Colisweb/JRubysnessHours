package com.colisweb.jrubysnesshours.core

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalTime

class TimeIntervalMergeSpec extends AnyWordSpec with Matchers {

  "Merging two intervals" in {
    val intervals = List(TimeInterval("08:00", "18:00"), TimeInterval("14:00", "16:00"))
    TimeInterval.mergeIntervals(intervals) shouldEqual (List(TimeInterval("08:00", "18:00")))
  }

  "Merging three intervals" in {
    val intervals = List(TimeInterval("08:00", "10:00"), TimeInterval("11:00", "20:00"), TimeInterval("10:00", "16:00"))
    TimeInterval.mergeIntervals(intervals) shouldEqual (List(TimeInterval("08:00", "20:00")))
  }

  "Non overlapping intervals" in {
    val intervals = List(TimeInterval("15:00", "16:00"), TimeInterval("18:00", "19:00"), TimeInterval("08:00", "10:00"))
    TimeInterval.mergeIntervals(intervals) shouldEqual (List(
      TimeInterval("08:00", "10:00"),
      TimeInterval("15:00", "16:00"),
      TimeInterval("18:00", "19:00")
    ))
  }

  "Overlapping and non overlapping intervals" in {
    val intervals = List(TimeInterval("08:00", "11:00"), TimeInterval("14:00", "18:00"), TimeInterval("10:00", "12:00"))
    TimeInterval.mergeIntervals(intervals) shouldEqual (List(
      TimeInterval("08:00", "12:00"),
      TimeInterval("14:00", "18:00")
    ))
  }

  "With extreme values" in {
    val intervals = List(
      TimeInterval("08:00", "12:00"),
      TimeInterval(LocalTime.MIN, "11:00"),
      TimeInterval("14:00", "17:00"),
      TimeInterval("11:00", LocalTime.MAX)
    )
    TimeInterval.mergeIntervals(intervals) shouldEqual (List(TimeInterval(LocalTime.MIN, LocalTime.MAX)))
  }

  implicit private def toLocalTime(s: String): LocalTime = LocalTime.parse(s)
}
