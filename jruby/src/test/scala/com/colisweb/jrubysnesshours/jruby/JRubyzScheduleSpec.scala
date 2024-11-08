package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._
import java.time.{LocalTime, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, TimeInterval, TimeIntervalForWeekDay}
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule._
import com.colisweb.jrubysnesshours.jruby.SampleSchedule._
import com.colisweb.jrubysnesshours.jruby.SpecUtils._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JRubyzScheduleSpec extends AnyWordSpec with Matchers {
  "jrubySchedule" should {
    "split on single date" in {
      jrubySchedule.splitTimeSegments(
        "2019-05-06T11:50:39Z",
        "2019-05-06T16:17:39Z",
        2.hours,
        None
      ) shouldBe Array(
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T12:00Z[UTC]", "2019-05-06T14:00Z[UTC]"),
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T13:00Z[UTC]", "2019-05-06T15:00Z[UTC]"),
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T14:00Z[UTC]", "2019-05-06T16:00Z[UTC]")
      )
    }
  }

  "rubyToDateTimeInterval" should {
    "with UTC timezone the same day" in {
      val res = exception("2019-04-12T16:17:39Z", "2019-04-12T18:15:40Z")

      val expectedStart = "2019-04-12" at "16:17:39" -> "Europe/Paris"
      val expectedEnd   = "2019-04-12" at "18:15:40" -> "Europe/Paris"
      res shouldEqual DateTimeInterval(expectedStart.toLocalDateTime, expectedEnd.toLocalDateTime)
    }

    "with GMT+2 timezone the same day" in {
      val res = exception("2019-04-12T16:17:39+02:00", "2019-04-12T18:15:40+02:00")

      val expectedStart = "2019-04-12" at "16:17:39" -> "Europe/Paris"
      val expectedEnd   = "2019-04-12" at "18:15:40" -> "Europe/Paris"
      res shouldEqual DateTimeInterval(expectedStart.toLocalDateTime, expectedEnd.toLocalDateTime)
    }

    "UTC timezone and GMT+2 timezone should return the same DateTimeInterval for the same date/hours" in {
      val resFromUTC  = exception("2019-04-12T14:00:00Z", "2019-04-12T18:00:00Z")
      val resFromGMT2 = exception("2019-04-12T14:00:00+02:00", "2019-04-12T18:00:00+02:00")

      resFromUTC shouldEqual resFromGMT2
    }

    "If start and end are not the same day" in {
      val res = exception("2019-04-10T16:17:39Z", "2019-04-15T18:15:40Z")

      val expectedStart = "2019-04-10" at "16:17:39" -> "Europe/Paris"
      val expectedEnd   = "2019-04-15" at "18:15:40" -> "Europe/Paris"
      res shouldEqual DateTimeInterval(expectedStart.toLocalDateTime, expectedEnd.toLocalDateTime)
    }
  }

  "rubyToPlanning" should {
    "Return a valid TimeIntervalForWeekDay for Monday" in {
      val res = planningEntry(1, "16:17", "18:15")
      res shouldEqual TimeIntervalForWeekDay(MONDAY, "16:17" - "18:15")
    }

    "Return a valid TimeIntervalForWeekDay for Sunday" in {
      val res = planningEntry(0, "16:17", "18:15")
      res shouldEqual TimeIntervalForWeekDay(SUNDAY, "16:17" - "18:15")
    }
  }

  "addInterval" should {
    "add an interval to an empty list" in {
      addInterval(Array(), TimeInterval("08:00", "12:00")) shouldEqual List(TimeInterval("08:00", "12:00"))
    }

    "add a non overlapping interval" in {
      addInterval(Array(TimeInterval("14:00", "18:00")), TimeInterval("08:00", "12:00")) shouldEqual List(
        TimeInterval("08:00", "12:00"),
        TimeInterval("14:00", "18:00")
      )
    }

    "add an overlapping interval" in {
      addInterval(Array(TimeInterval("14:00", "18:00")), TimeInterval("08:00", "16:00")) shouldEqual List(
        TimeInterval("08:00", "18:00")
      )
    }
  }

  "bug found by Florian when timezone is different between schedule and splitTimeSegments arguments" when {
    val schedule = JRubyzSchedule.schedule(
      Array(TimeIntervalForWeekDay(MONDAY, TimeInterval("09:00", "20:00"))),
      Array.empty,
      "Europe/Paris"
    )

    "for UTC timezone" in {
      val start = "2015-03-02T10:00Z[Etc/UTC]"
      val end   = "2015-03-02T23:59:59.000999999Z[Etc/UTC]"

      schedule.timeSegments(start, end) shouldBe
        Array(
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T10:00Z[UTC]", "2015-03-02T19:00Z[UTC]")
        ) // 11h-20h French time

      schedule.splitTimeSegments(start, end, 3.hours, None) shouldBe
        Array(
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T10:00Z[UTC]", "2015-03-02T13:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T11:00Z[UTC]", "2015-03-02T14:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T12:00Z[UTC]", "2015-03-02T15:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T13:00Z[UTC]", "2015-03-02T16:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T14:00Z[UTC]", "2015-03-02T17:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T15:00Z[UTC]", "2015-03-02T18:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T16:00Z[UTC]", "2015-03-02T19:00Z[UTC]")
        )
    }

    "for EuropeParis timezone" in {
      val start = "2015-03-02T10:00+01:00[Europe/Paris]"
      val end   = "2015-03-02T23:59:59.000999999+01:00[Europe/Paris]"

      schedule.timeSegments(start, end) shouldBe
        Array(RubyTimeSegmentInterval("2015-03-02", "2015-03-02T09:00Z[UTC]", "2015-03-02T19:00Z[UTC]"))

      schedule.splitTimeSegments(start, end, 3.hours, None) shouldBe
        Array(
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T09:00Z[UTC]", "2015-03-02T12:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T10:00Z[UTC]", "2015-03-02T13:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T11:00Z[UTC]", "2015-03-02T14:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T12:00Z[UTC]", "2015-03-02T15:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T13:00Z[UTC]", "2015-03-02T16:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T14:00Z[UTC]", "2015-03-02T17:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T15:00Z[UTC]", "2015-03-02T18:00Z[UTC]"),
          RubyTimeSegmentInterval("2015-03-02", "2015-03-02T16:00Z[UTC]", "2015-03-02T19:00Z[UTC]")
        )
    }
  }

  implicit private def toLocalTime(s: String): LocalTime = LocalTime.parse(s)
  implicit private def toZDT(s: String): ZonedDateTime   = ZonedDateTime.parse(s)
}
