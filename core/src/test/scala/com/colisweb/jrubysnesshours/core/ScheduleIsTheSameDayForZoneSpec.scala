package com.colisweb.jrubysnesshours.core

import java.time.ZoneId

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleIsTheSameDayForZoneSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  "Schedule#isTheSameDayForZone" when {

    "Same day same zone" in {
      Schedule.isTheSameDayForZone(
        "2019-05-02" at "10:00" -> "UTC",
        "2019-05-02" at "10:00" -> "UTC",
        ZoneId.of("UTC")
      ) shouldBe true
    }

    "Not the same date" in {
      Schedule.isTheSameDayForZone(
        "2019-05-02" at "10:00" -> "UTC",
        "2019-05-03" at "10:00" -> "UTC",
        ZoneId.of("UTC")
      ) shouldBe false
    }

    "Not in the same zone but still in same day" in {
      Schedule.isTheSameDayForZone(
        "2019-05-03" at "10:00" -> "UTC",
        "2019-05-03" at "10:00" -> "+01:00",
        ZoneId.of("+02:00")
      ) shouldBe true
    }

    "The day depend on the zone" in {
      Schedule.isTheSameDayForZone(
        "2019-05-03" at "00:01" -> "UTC",
        "2019-05-03" at "00:01" -> "+02:00",
        ZoneId.of("+01:00")
      ) shouldBe false

      Schedule.isTheSameDayForZone(
        "2019-05-02" at "00:01" -> "UTC",
        "2019-05-03" at "00:01" -> "+02:00",
        ZoneId.of("+01:00")
      ) shouldBe true
    }
  }
}
