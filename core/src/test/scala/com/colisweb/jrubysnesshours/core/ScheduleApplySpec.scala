package com.colisweb.jrubysnesshours.core
import java.time.DayOfWeek._
import java.time.ZoneOffset.UTC

import org.scalatest.{Matchers, WordSpec}

class ScheduleApplySpec extends WordSpec with Matchers {

  import SpecUtils._

  "Schedule.apply" should {

    "index non-overlapping exceptions by day" in {
      val rawExceptions = List(
        "2019-03-15" :- "10:00" to "2019-03-15" :- "12:00",
        "2019-03-15" :- "13:00" to "2019-03-15" :- "16:00",
        "2019-03-16" :- "10:00" to "2019-03-16" :- "18:00",
        "2019-03-17" :- "15:00" to "2019-03-17" :- "16:00",
        "2019-03-17" :- "18:00" to "2019-03-17" :- "19:00",
        "2019-03-17" :- "08:00" to "2019-03-17" :- "10:00",
        "2019-03-19" :- "17:00" to "2019-03-19" :- "21:00",
        "2019-03-19" :- "10:00" to "2019-03-19" :- "12:00"
      ).sortBy(_.hashCode())

      Schedule(Nil, rawExceptions, UTC).exceptions should contain theSameElementsAs Map(
        "2019-03-15" at List("10:00" - "12:00", "13:00" - "16:00"),
        "2019-03-16" at List("10:00" - "18:00"),
        "2019-03-17" at List("08:00" - "10:00", "15:00" - "16:00", "18:00" - "19:00"),
        "2019-03-19" at List("10:00" - "12:00", "17:00" - "21:00")
      )
    }

    "index overlapping exceptions by day" in {
      val rawExceptions = List(
        "2019-03-15" :- "10:00" to "2019-03-15" :- "16:00",
        "2019-03-15" :- "13:00" to "2019-03-15" :- "19:00",
        "2019-03-16" :- "10:00" to "2019-03-17" :- "02:00",
        "2019-03-17" :- "01:00" to "2019-03-17" :- "03:00",
        "2019-03-17" :- "11:00" to "2019-03-17" :- "15:00",
        "2019-03-17" :- "12:00" to "2019-03-17" :- "15:00",
        "2019-03-19" :- "08:00" to "2019-03-19" :- "17:00",
        "2019-03-19" :- "08:00" to "2019-03-19" :- "12:00"
      ).sortBy(_.hashCode())

      Schedule(Nil, rawExceptions, UTC).exceptions should contain theSameElementsAs Map(
        "2019-03-15" at List("10:00" - "19:00"),
        "2019-03-16" at List("10:00" - "23:59"),
        "2019-03-17" at List("00:00" - "03:00", "11:00" - "15:00"),
        "2019-03-19" at List("08:00" - "17:00")
      )
    }

    "index exceptions over more than one month" in {
      val initDate              = "2019-01-01".toLocalDate
      val dates                 = (1 until 100).toList.map(i => initDate.plusDays(i.toLong))
      val rawExceptions         = dates.map(date => date :- "10:00" to date :- "18:00")
      val expectedTimeIntervals = List("10:00" - "18:00")
      val expected              = dates.map(_ -> expectedTimeIntervals).toMap

      val result = Schedule(Nil, rawExceptions, UTC).exceptions

      result should contain theSameElementsAs expected
    }

    "index non-overlapping intervals by day-of-week" in {
      val rawPlanning = List(
        MONDAY at "10:00" - "12:00",
        MONDAY at "14:00" - "18:00",
        WEDNESDAY at "08:00" - "19:00",
        THURSDAY at "09:00" - "10:00",
        THURSDAY at "13:00" - "15:00",
        THURSDAY at "17:15" - "18:15",
        SATURDAY at "09:15" - "17:45"
      ).sortBy(_.hashCode())

      Schedule(rawPlanning, Nil, UTC).planning should contain theSameElementsAs Map(
        MONDAY    -> List("10:00" - "12:00", "14:00" - "18:00"),
        WEDNESDAY -> List("08:00" - "19:00"),
        THURSDAY  -> List("09:00" - "10:00", "13:00" - "15:00", "17:15" - "18:15"),
        SATURDAY  -> List("09:15" - "17:45")
      )
    }

    "index overlapping intervals by day-of-week" in {
      val rawPlanning = List(
        MONDAY at "10:00" - "16:00",
        MONDAY at "14:00" - "18:00",
        WEDNESDAY at "08:00" - "19:00",
        WEDNESDAY at "10:00" - "19:00",
        THURSDAY at "09:00" - "13:00",
        THURSDAY at "12:00" - "15:00",
        THURSDAY at "12:15" - "18:15",
        THURSDAY at "20:00" - "21:15"
      ).sortBy(_.hashCode())

      Schedule(rawPlanning, Nil, UTC).planning should contain theSameElementsAs Map(
        MONDAY    -> List("10:00" - "18:00"),
        WEDNESDAY -> List("08:00" - "19:00"),
        THURSDAY  -> List("09:00" - "18:15", "20:00" - "21:15")
      )
    }
  }

}
