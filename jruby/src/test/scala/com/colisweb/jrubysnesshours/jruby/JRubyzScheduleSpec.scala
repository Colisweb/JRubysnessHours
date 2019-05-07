package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._
import java.time.ZonedDateTime

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, TimeIntervalForWeekDay}
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule._
import org.scalatest.{Matchers, WordSpec}

class JRubyzScheduleSpec extends WordSpec with Matchers {

  import SpecUtils._

  "jrubySchedule" should {
    val jrubySchedule =
      schedule(
        Array(
          planningEntry(1, "12:00", "18:00"),
          planningEntry(2, "12:00", "18:00"),
          planningEntry(3, "12:00", "18:00"),
          planningEntry(4, "12:00", "18:00"),
          planningEntry(5, "12:00", "18:00"),
          planningEntry(6, "12:00", "18:00"),
        ),
        Array(exception("2019-05-08T14:00:00Z", "2019-05-08T18:00:00Z")),
        "UTC"
      )

    "split on single date" in {
      jrubySchedule.splitTimeSegments(
        ZonedDateTime.parse("2019-05-06T11:50:39Z"),
        ZonedDateTime.parse("2019-05-06T16:17:39Z"),
        2
      ) shouldBe Array(
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T12:00Z[UTC]", "2019-05-06T14:00Z[UTC]"),
        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T14:00Z[UTC]", "2019-05-06T16:00Z[UTC]"),
      )
    }

    "split on 2 weeks" in {
      //TODO : fix assertion
//      jrubySchedule.splitTimeSegments(
//        ZonedDateTime.parse("2019-05-06T11:50:39Z"),
//        ZonedDateTime.parse("2019-05-20T16:17:39Z"),
//        2
//      ) shouldBe Array(
//        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T12:00Z[UTC]", "2019-05-06T14:00Z[UTC]"),
//        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T14:00Z[UTC]", "2019-05-06T16:00Z[UTC]"),
//        RubyTimeSegmentInterval("2019-05-06", "2019-05-06T16:00Z[UTC]", "2019-05-06T18:00Z[UTC]")
//      )
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

  "rubyWeekDayToJavaWeekDay" should {
    "for MONDAY" in {
      rubyWeekDayToJavaWeekDay(1) shouldEqual MONDAY
    }

    "for TUESDAY" in {
      rubyWeekDayToJavaWeekDay(2) shouldEqual TUESDAY
    }

    "for WEDNESDAY" in {
      rubyWeekDayToJavaWeekDay(3) shouldEqual WEDNESDAY
    }

    "for THURSDAY" in {
      rubyWeekDayToJavaWeekDay(4) shouldEqual THURSDAY
    }

    "for FRIDAY" in {
      rubyWeekDayToJavaWeekDay(5) shouldEqual FRIDAY
    }

    "for SATURDAY" in {
      rubyWeekDayToJavaWeekDay(6) shouldEqual SATURDAY
    }

    "for SUNDAY" in {
      rubyWeekDayToJavaWeekDay(0) shouldEqual SUNDAY
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
