package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, TimeIntervalForWeekDay}
import org.scalatest.{Matchers, WordSpec}

class JRubyzScheduleSpec extends WordSpec with Matchers {

  import com.colisweb.jrubysnesshours.jruby.UtilsSpec._

  "rubyToDateTimeInterval" should {

    "with UTC timezone the same day" in {
      val res = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T16:17:39Z", "2019-04-12T18:15:40Z")

      val expectedStart = parseDate("2019-04-12").atTime("16:17:39".toLocalTime)
      val expectedEnd   = parseDate("2019-04-12").atTime("18:15:40".toLocalTime)
      res shouldEqual DateTimeInterval(expectedStart, expectedEnd)
    }

    "with GMT+2 timezone the same day" in {
      val res = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T16:17:39+02:00", "2019-04-12T18:15:40+02:00")

      val expectedStart = parseDate("2019-04-12").atTime("16:17:39".toLocalTime)
      val expectedEnd   = parseDate("2019-04-12").atTime("18:15:40".toLocalTime)
      res shouldEqual DateTimeInterval(expectedStart, expectedEnd)
    }

    "UTC timezone and GMT+2 timezone should return the same DateTimeInterval for the same date/hours" in {
      val resFromUTC  = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T14:00:00Z", "2019-04-12T18:00:00Z")
      val resFromGMT2 = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T14:00:00+02:00", "2019-04-12T18:00:00+02:00")

      resFromUTC shouldEqual resFromGMT2
    }

    "If start and end are not the same day" in {
      val res = JRubyzSchedule.rubyToDateTimeInterval("2019-04-10T16:17:39Z", "2019-04-15T18:15:40Z")

      val expectedStart = parseDate("2019-04-10").atTime("16:17:39".toLocalTime)
      val expectedEnd   = parseDate("2019-04-15").atTime("18:15:40".toLocalTime)
      res shouldEqual DateTimeInterval(expectedStart, expectedEnd)
    }
  }

  "rubyWeekDayToJavaWeekDay" should {
    "for MONDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(1) shouldEqual DayOfWeek.MONDAY
    }

    "for TUESDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(2) shouldEqual DayOfWeek.TUESDAY
    }

    "for WEDNESDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(3) shouldEqual DayOfWeek.WEDNESDAY
    }

    "for THURSDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(4) shouldEqual DayOfWeek.THURSDAY
    }

    "for FRIDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(5) shouldEqual DayOfWeek.FRIDAY
    }

    "for SATURDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(6) shouldEqual DayOfWeek.SATURDAY
    }

    "for SUNDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(0) shouldEqual DayOfWeek.SUNDAY
    }
  }

  "rubyToPlanning" should {
    "Return a valid TimeIntervalForWeekDay for Monday" in {
      val res = JRubyzSchedule.rubyToPlanning(1, "16:17", "18:15")
      res shouldEqual TimeIntervalForWeekDay(DayOfWeek.MONDAY, parseInterval("16:17", "18:15"))
    }

    "Return a valid TimeIntervalForWeekDay for Sunday" in {
      val res = JRubyzSchedule.rubyToPlanning(0, "16:17", "18:15")
      res shouldEqual TimeIntervalForWeekDay(DayOfWeek.SUNDAY, parseInterval("16:17", "18:15"))
    }
  }

}
