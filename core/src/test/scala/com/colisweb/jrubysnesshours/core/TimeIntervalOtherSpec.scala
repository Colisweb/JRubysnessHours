package com.colisweb.jrubysnesshours.core

import java.time.ZonedDateTime

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TimeIntervalOtherSpec extends AnyWordSpec with Matchers {
  "TimeInterval#roundToFullHours" when {
    "11:00-12:00 stays same" in {
      val interval = "11:00" - "12:00"
      interval.roundToMinutes(60) shouldBe Some(interval)
    }

    "10:01-12:59 rounds to 11:00-12:00" in {
      val interval = "10:01" - "12:59"
      interval.roundToMinutes(60) shouldBe Some("11:00" - "12:00")
    }

    "10:01-10:59 rounds to None" in {
      val interval = "10:01" - "10:59"
      interval.roundToMinutes(60) shouldBe empty
    }
  }

  "TimeInterval#roundToFullMinutes(30)" when {
    "11:00-12:00 stays same" in {
      val interval = "11:00" - "12:00"
      interval.roundToMinutes(30) shouldBe Some(interval)
    }

    "10:01-12:59 rounds to 10:30-12:30" in {
      val interval = "10:01" - "12:59"
      interval.roundToMinutes(30) shouldBe Some("10:30" - "12:30")
    }

    "with seconds rounds to 11:30-16:30" in {
      val interval = TimeInterval(
        ZonedDateTime.parse("2019-05-06T11:20:39Z").toLocalTime,
        ZonedDateTime.parse("2019-05-07T16:47:39Z").toLocalTime
      )
      interval.roundToMinutes(30) shouldBe Some("11:30" - "16:30")
    }

    "10:01-10:59 rounds to None" in {
      val interval = "10:01" - "10:59"
      interval.roundToMinutes(30) shouldBe empty
    }
  }

  "TimeInterval#roundToFullMinutes(90)" when {
    "12:20 - 16:47 rounds to 13:00 - 16:30" in {
      val interval = "12:20" - "16:47"
      interval.roundToMinutes(90) shouldBe Some("13:00" - "16:30")
    }
  }

  "TimeInterval#roundToFullMinutes(2h15)" when {
    "12:20 - 16:47 rounds to 13:00 - 16:45" in {
      val interval = "12:20" - "16:47"
      interval.roundToMinutes(135) shouldBe Some("13:00" - "16:45")
    }
  }

  "TimeInterval#split(2h)" when {
    "11:00-12:00 is empty" in {
      val interval = "11:00" - "12:00"
      interval.split(2.hours) shouldBe empty
    }

    "11:05-13:15 is empty" in {
      val interval = "11:05" - "13:15"
      interval.split(2.hours) shouldBe empty
    }

    "10:00-12:00 splits to List(10-12)" in {
      val interval = "10:00" - "12:00"
      interval.split(2.hours) shouldBe List(interval)
    }

    "10:00-14:00 splits to List(10-12, 11-13, 12-14)" in {
      val interval = "10:00" - "14:00"
      interval.split(2.hours) shouldBe List("10:00" - "12:00", "11:00" - "13:00", "12:00" - "14:00")
    }

    "10:00-15:00 splits to List(10-12, 11-13, 12-14, 13-15)" in {
      val interval = "10:00" - "15:00"
      interval.split(2.hours) shouldBe List(
        "10:00" - "12:00",
        "11:00" - "13:00",
        "12:00" - "14:00",
        "13:00" - "15:00"
      )
    }
  }

  "TimeInterval#split(30min)" when {
    "11:00-11:15 is empty" in {
      val interval = "11:00" - "11:15"
      interval.split(30.minutes) shouldBe empty
    }

    "10:00-11:15 splits to 2 segments" in {
      val interval = "10:00" - "11:15"
      interval.split(30.minutes) shouldBe List("10:00" - "10:30", "10:30" - "11:00")
    }

    "10:10-11:10 splits to 10:30-11:00" in {
      val interval = "10:10" - "11:10"
      interval.split(30.minutes) shouldBe List("10:30" - "11:00")
    }
  }

  "TimeInterval#split(20min)" when {
    "11:00-11:15 is empty" in {
      val interval = "11:00" - "11:15"
      interval.split(20.minutes) shouldBe empty
    }

    "10:05-11:15 splits to 2 segments" in {
      val interval = "10:05" - "11:15"
      interval.split(20.minutes) shouldBe List("10:20" - "10:40", "10:40" - "11:00")
    }
  }
}
