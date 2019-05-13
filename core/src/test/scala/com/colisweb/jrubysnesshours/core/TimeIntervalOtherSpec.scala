package com.colisweb.jrubysnesshours.core

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}
class TimeIntervalOtherSpec extends WordSpec with Matchers {

  "TimeInterval#roundToFullHours" when {

    "11:00-12:00 stays same" in {
      val interval = "11:00" - "12:00"
      interval.roundToFullHours shouldBe Some(interval)
    }

    "10:01-12:59 rounds to 11:00-12:00" in {
      val interval = "10:01" - "12:59"
      interval.roundToFullHours shouldBe Some("11:00" - "12:00")
    }

    "10:01-10:59 rounds to None" in {
      val interval = "10:01" - "10:59"
      interval.roundToFullHours shouldBe empty
    }

  }

  "TimeInterval#split(2h)" when {

    "11:00-12:00 is empty" in {
      val interval = "11:00" - "12:00"
      interval.split(2) shouldBe empty
    }

    "10:00-12:00 splits to List(10-12)" in {
      val interval = "10:00" - "12:00"
      interval.split(2) shouldBe List(interval)
    }

    "10:00-14:00 splits to List(10-12, 11-13, 12-14)" in {
      val interval = "10:00" - "14:00"
      interval.split(2) shouldBe List("10:00" - "12:00", "11:00" - "13:00", "12:00" - "14:00")
    }

    "10:00-15:00 splits to List(10-12, 11-13, 12-14, 13-15)" in {
      val interval = "10:00" - "15:00"
      interval.split(2) shouldBe List("10:00" - "12:00", "11:00" - "13:00", "12:00" - "14:00", "13:00" - "15:00")
    }

  }

}
