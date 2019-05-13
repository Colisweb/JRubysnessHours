package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._
import java.time.ZonedDateTime

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, TimeIntervalForWeekDay}
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule._
import com.colisweb.jrubysnesshours.jruby.SampleSchedule._
import com.colisweb.jrubysnesshours.jruby.SpecUtils._
import org.scalatest.{Matchers, WordSpec}

class JRubyzScheduleSpec extends WordSpec with Matchers {

  "jrubySchedule" should {
    "split on single date" in {
      jrubySchedule.splitTimeSegments(
        ZonedDateTime.parse("2019-05-06T11:50:39Z"),
        ZonedDateTime.parse("2019-05-06T16:17:39Z"),
        2
      ) shouldBe Array(
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T12:00Z[UTC]", "2019-05-06T14:00Z[UTC]"),
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T13:00Z[UTC]", "2019-05-06T15:00Z[UTC]"),
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T14:00Z[UTC]", "2019-05-06T16:00Z[UTC]"),
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

}
