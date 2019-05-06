package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek.THURSDAY

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleSplitTimeSegmentsSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {
  val aThursday = "2019-05-02".toLocalDate
  val aTuesday  = "2019-05-07".toLocalDate

  "a Schedule with an empty planning" when {
    val emptySchedule = schedule.copy(planning = Map.empty)

    "has an empty list of split segments" in {
      emptySchedule.splitTimeSegments(aThursday) shouldBe Nil
    }
  }

  "a Schedule" when {
    "has a list of split segments" in {
      schedule.splitTimeSegments(aThursday) shouldBe
        List("10:00" - "12:00", "12:00" - "14:00", "14:00" - "16:00", "16:00" - "18:00")
      schedule.splitTimeSegments(aTuesday) shouldBe
        List("10:00" - "12:00", "12:00" - "14:00", "15:00" - "17:00", "17:00" - "19:00")
    }
  }

  "a Schedule" when {
    "splits when the day is cut 9-11 11-15" in {
      schedule
        .copy(planning = planning.updated(THURSDAY, List("09:00" - "11:00", "11:00" - "15:00")))
        .splitTimeSegments(aThursday) shouldBe
        List("09:00" - "11:00", "11:00" - "13:00", "13:00" - "15:00")

    }
    "splits when the day is cut 9-12 12-15" in {
      schedule
        .copy(planning = planning.updated(THURSDAY, List("09:00" - "12:00", "12:00" - "15:00")))
        .splitTimeSegments(aThursday) shouldBe
        List("09:00" - "11:00", "12:00" - "14:00")

    }
  }

  "a Schedule with exceptions" when {
    val scheduleWithExceptions =
      schedule.copy(exceptions = Map(aThursday -> List("12:00" - "13:00", "16:00" - "23:00")))

    "splits when the day is cut 10-12 13-16" in {
      scheduleWithExceptions.splitTimeSegments(aThursday) shouldBe
        List("10:00" - "12:00", "13:00" - "15:00")

    }
    "splits in 1h slots when the day is cut 10-12 13-16" in {
      scheduleWithExceptions.splitTimeSegments(aThursday, 1) shouldBe
        List("10:00" - "11:00", "11:00" - "12:00", "13:00" - "14:00", "14:00" - "15:00", "15:00" - "16:00")

    }
    "splits in 3h slots when the day is cut 10-12 13-16" in {
      scheduleWithExceptions.splitTimeSegments(aThursday, 3) shouldBe
        List("13:00" - "16:00")

    }
  }
}
