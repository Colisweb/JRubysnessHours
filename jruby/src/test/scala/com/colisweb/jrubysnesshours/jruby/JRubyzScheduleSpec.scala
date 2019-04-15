package com.colisweb.jrubysnesshours.jruby

import java.time.{LocalDate, LocalTime}

import com.colisweb.jrubysnesshours.core.DateTimeInterval
import org.scalatest.{Matchers, WordSpec}

class JRubyzScheduleSpec extends WordSpec with Matchers {

  def aDate(date: String): LocalDate = // TODO write a spec util class to use it in core and jruby
    LocalDate.parse(date)

  implicit class StringToLocalTime(str: String) { // TODO write a spec util class to use it in core and jruby
    def toLocalTime: LocalTime = LocalTime.parse(str)
  }

  "rubyToDateTimeInterval" should {

    "with UTC timezone the same day" in {
      val res = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T16:17:39Z", "2019-04-12T18:15:40Z")

      val expectedStart = aDate("2019-04-12").atTime("16:17:39".toLocalTime)
      val expectedEnd = aDate("2019-04-12").atTime("18:15:40".toLocalTime)
      res shouldEqual DateTimeInterval(expectedStart, expectedEnd)
    }

    "with GMT+2 timezone the same day" in {
      val res = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T16:17:39+02:00", "2019-04-12T18:15:40+02:00")

      val expectedStart = aDate("2019-04-12").atTime("16:17:39".toLocalTime)
      val expectedEnd = aDate("2019-04-12").atTime("18:15:40".toLocalTime)
      res shouldEqual DateTimeInterval(expectedStart, expectedEnd)
    }

    "UTC timezone and GMT+2 timezone should return the same DateTimeInterval for the same date/hours" in {
      val resFromUTC = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T14:00:00Z", "2019-04-12T18:00:00Z")
      val resFromGMT2 = JRubyzSchedule.rubyToDateTimeInterval("2019-04-12T14:00:00+02:00", "2019-04-12T18:00:00+02:00")

      resFromUTC shouldEqual resFromGMT2
    }

    "If start and end are not the same day" in {
      val res = JRubyzSchedule.rubyToDateTimeInterval("2019-04-10T16:17:39Z", "2019-04-15T18:15:40Z")

      val expectedStart = aDate("2019-04-10").atTime("16:17:39".toLocalTime)
      val expectedEnd = aDate("2019-04-15").atTime("18:15:40".toLocalTime)
      res shouldEqual DateTimeInterval(expectedStart, expectedEnd)
    }

  }

}