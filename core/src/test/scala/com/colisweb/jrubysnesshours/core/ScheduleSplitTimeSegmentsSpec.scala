package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek.THURSDAY

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleSplitTimeSegmentsSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {
  private val aThursday = "2019-05-02".toLocalDate
  private val aTuesday  = "2019-05-07".toLocalDate

  "a Schedule with an empty planning" when {
    val emptySchedule = schedule.copy(planning = Map.empty)

    "has an empty list of split segments" in {
      emptySchedule.splitTimeSegmentsSingleDate(aThursday, 2) shouldBe Nil
    }
  }

  "a Schedule" when {
    "has a list of split segments" in {
      schedule.splitTimeSegmentsSingleDate(aThursday, 2) shouldBe
        List(
          "10:00" - "12:00",
          "11:00" - "13:00",
          "12:00" - "14:00",
          "13:00" - "15:00",
          "14:00" - "16:00",
          "15:00" - "17:00",
          "16:00" - "18:00",
          "17:00" - "19:00",
        )
      schedule.splitTimeSegmentsSingleDate(aTuesday, 2) shouldBe
        List(
          "10:00" - "12:00",
          "11:00" - "13:00",
          "12:00" - "14:00",
          "15:00" - "17:00",
          "16:00" - "18:00",
          "17:00" - "19:00"
        )
    }

    "has a list of split segments for a time range" in {
      schedule.splitTimeSegments(
        "2019-05-02" at "10:00" -> FRANCE_TIMEZONE,
        "2019-05-03" at "18:00" -> FRANCE_TIMEZONE,
        2
      ) shouldBe
        List(
          "2019-05-02" at "10:00" - "12:00",
          "2019-05-02" at "11:00" - "13:00",
          "2019-05-02" at "12:00" - "14:00",
          "2019-05-02" at "13:00" - "15:00",
          "2019-05-02" at "14:00" - "16:00",
          "2019-05-02" at "15:00" - "17:00",
          "2019-05-02" at "16:00" - "18:00",
          "2019-05-02" at "17:00" - "19:00",
          "2019-05-03" at "10:00" - "12:00",
          "2019-05-03" at "11:00" - "13:00",
          "2019-05-03" at "12:00" - "14:00",
          "2019-05-03" at "13:00" - "15:00",
          "2019-05-03" at "14:00" - "16:00",
          "2019-05-03" at "15:00" - "17:00",
          "2019-05-03" at "16:00" - "18:00",
        )
    }
  }

  "a Schedule" when {
    "splits when the day is cut 9-11 11-15" in {
      schedule
        .copy(planning = planning.updated(THURSDAY, List("09:00" - "11:00", "11:00" - "15:00")))
        .splitTimeSegmentsSingleDate(aThursday, 2) shouldBe
        List("09:00" - "11:00", "11:00" - "13:00", "12:00" - "14:00", "13:00" - "15:00")
    }

    "splits when the day is cut 9-12 12-15" in {
      schedule
        .copy(planning = planning.updated(THURSDAY, List("09:00" - "12:00", "12:00" - "15:00")))
        .splitTimeSegmentsSingleDate(aThursday, 2) shouldBe
        List("09:00" - "11:00", "10:00" - "12:00", "12:00" - "14:00", "13:00" - "15:00")
    }

    "splits when the day is cut 9-12 12-15 and THURSDAY is empty" in {
      schedule
        .copy(planning = planning.updated(THURSDAY, List.empty))
        .splitTimeSegmentsSingleDate(aThursday, 2) shouldBe List.empty
    }

    "splits when the day is cut 9-12 12-15 and THURSDAY is empty and asking for 2019-05-02 at 12:01 to 23:59 the same day" in {
      val cutOff = Some(
        DoubleCutOff(
          sameDay = CutOff(limit = "09:00".toLocalTime, firstAvailableTime = "12:00".toLocalTime),
          nextDay = CutOff(limit = "12:00".toLocalTime, firstAvailableTime = "15:00".toLocalTime)
        )
      )

      schedule
        .copy(planning = planning.updated(THURSDAY, List.empty))
        .splitTimeSegmentsSingleDate(aThursday, 2) shouldBe List.empty

      val slots =
        schedule
          .copy(planning = planning.updated(THURSDAY, List.empty))
          .splitTimeSegments(
            "2019-05-02" at "12:01" -> FRANCE_TIMEZONE, // it's a THURSDAY
            "2019-05-02" at "23:59" -> FRANCE_TIMEZONE, // it's a THURSDAY
            2,
            cutOff
          )
      slots shouldBe List(
        "2019-05-03" at "15:00" - "17:00",
        "2019-05-03" at "16:00" - "18:00",
        "2019-05-03" at "17:00" - "19:00",
      )
    }
  }

  "a Schedule with exceptions" when {
    val scheduleWithExceptions =
      schedule.copy(exceptions = Map(aThursday -> List("12:00" - "13:00", "16:00" - "23:00")))

    "splits when the day is cut 10-12 13-16" in {
      scheduleWithExceptions.splitTimeSegmentsSingleDate(aThursday, 2) shouldBe
        List("10:00" - "12:00", "13:00" - "15:00", "14:00" - "16:00")
    }

    "splits in 1h slots when the day is cut 10-12 13-16" in {
      scheduleWithExceptions.splitTimeSegmentsSingleDate(aThursday, 1) shouldBe
        List("10:00" - "11:00", "11:00" - "12:00", "13:00" - "14:00", "14:00" - "15:00", "15:00" - "16:00")
    }

    "splits in 3h slots when the day is cut 10-12 13-16" in {
      scheduleWithExceptions.splitTimeSegmentsSingleDate(aThursday, 3) shouldBe
        List("13:00" - "16:00")
    }
  }
}
