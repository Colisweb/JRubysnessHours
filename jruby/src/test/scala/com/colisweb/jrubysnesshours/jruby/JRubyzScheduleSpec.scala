package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._

import com.colisweb.jrubysnesshours.core.{DateTimeInterval, TimeIntervalForWeekDay}
import org.scalatest.{Matchers, WordSpec}

class JRubyzScheduleSpec extends WordSpec with Matchers {

  import SpecUtils._

  "rubyToDateTimeInterval" should {

    "with UTC timezone the same day" in {
      val res = JRubyzSchedule.dateTimeInterval("2019-04-12T16:17:39Z", "2019-04-12T18:15:40Z")

      val expectedStart = "2019-04-12" at "16:17:39" -> "Europe/Paris"
      val expectedEnd   = "2019-04-12" at "18:15:40" -> "Europe/Paris"
      res shouldEqual DateTimeInterval(expectedStart.toLocalDateTime, expectedEnd.toLocalDateTime)
    }

    "with GMT+2 timezone the same day" in {
      val res = JRubyzSchedule.dateTimeInterval("2019-04-12T16:17:39+02:00", "2019-04-12T18:15:40+02:00")

      val expectedStart = "2019-04-12" at "16:17:39" -> "Europe/Paris"
      val expectedEnd   = "2019-04-12" at "18:15:40" -> "Europe/Paris"
      res shouldEqual DateTimeInterval(expectedStart.toLocalDateTime, expectedEnd.toLocalDateTime)
    }

    "UTC timezone and GMT+2 timezone should return the same DateTimeInterval for the same date/hours" in {
      val resFromUTC  = JRubyzSchedule.dateTimeInterval("2019-04-12T14:00:00Z", "2019-04-12T18:00:00Z")
      val resFromGMT2 = JRubyzSchedule.dateTimeInterval("2019-04-12T14:00:00+02:00", "2019-04-12T18:00:00+02:00")

      resFromUTC shouldEqual resFromGMT2
    }

    "If start and end are not the same day" in {
      val res = JRubyzSchedule.dateTimeInterval("2019-04-10T16:17:39Z", "2019-04-15T18:15:40Z")

      val expectedStart = "2019-04-10" at "16:17:39" -> "Europe/Paris"
      val expectedEnd   = "2019-04-15" at "18:15:40" -> "Europe/Paris"
      res shouldEqual DateTimeInterval(expectedStart.toLocalDateTime, expectedEnd.toLocalDateTime)
    }
  }

  "rubyWeekDayToJavaWeekDay" should {
    "for MONDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(1) shouldEqual MONDAY
    }

    "for TUESDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(2) shouldEqual TUESDAY
    }

    "for WEDNESDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(3) shouldEqual WEDNESDAY
    }

    "for THURSDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(4) shouldEqual THURSDAY
    }

    "for FRIDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(5) shouldEqual FRIDAY
    }

    "for SATURDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(6) shouldEqual SATURDAY
    }

    "for SUNDAY" in {
      JRubyzSchedule.rubyWeekDayToJavaWeekDay(0) shouldEqual SUNDAY
    }
  }

  "rubyToPlanning" should {
    "Return a valid TimeIntervalForWeekDay for Monday" in {
      val res = JRubyzSchedule.planning(1, "16:17", "18:15")
      res shouldEqual TimeIntervalForWeekDay(MONDAY, "16:17" - "18:15")
    }

    "Return a valid TimeIntervalForWeekDay for Sunday" in {
      val res = JRubyzSchedule.planning(0, "16:17", "18:15")
      res shouldEqual TimeIntervalForWeekDay(SUNDAY, "16:17" - "18:15")
    }
  }

}
