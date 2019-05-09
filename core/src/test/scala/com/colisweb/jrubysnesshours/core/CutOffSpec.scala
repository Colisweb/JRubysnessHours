package com.colisweb.jrubysnesshours.core

import java.time.LocalTime

import org.scalatest.{Matchers, WordSpec}

class CutOffSpec extends WordSpec with Matchers {

  import SpecUtils._

  "DoubleCutOff" should {
    val doubleCutOff = DoubleCutOff(
      sameDay = CutOff(limit = "08:00", firstAvailableTime = "18:00"),
      nextDay = CutOff(limit = "12:00", firstAvailableTime = "15:00")
    )

    "not cut before first limit" in {
      doubleCutOff.nextAvailableMoment("06:59") shouldBe
        AvailableFrom(availableTime = "06:59")
    }

    "cut on the same day between 2 limits" in {
      doubleCutOff.nextAvailableMoment("08:01") shouldBe
        AvailableFrom(availableTime = "18:00")
    }

    "cut on the next day after 2nd limit" in {
      doubleCutOff.nextAvailableMoment("12:01") shouldBe
        AvailableFrom(availableTime = "15:00", sameDay = false)
    }
  }

  implicit private def toLocalTime(s: String): LocalTime = s.toLocalTime
}
