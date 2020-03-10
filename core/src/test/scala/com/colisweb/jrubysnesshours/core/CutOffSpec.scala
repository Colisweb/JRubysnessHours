package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalTime}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CutOffSpec extends AnyWordSpec with Matchers {
  import SpecUtils._

  "DoubleCutOff" should {
    val doubleCutOff = DoubleCutOff(
      sameDay = CutOff(limit = "08:00", firstAvailableTime = "18:00"),
      nextDay = CutOff(limit = "12:00", firstAvailableTime = "15:00")
    )

    val sameDay: LocalDate = "2019-03-15"
    val nextDay: LocalDate = "2019-03-20"

    "not cut before first limit" in {
      doubleCutOff.nextAvailableMoment("06:59", sameDay, nextDay) shouldBe
        "2019-03-15" :- "06:59"
    }

    "cut on the same day between 2 limits" in {
      doubleCutOff.nextAvailableMoment("08:01", sameDay, nextDay) shouldBe
        "2019-03-15" :- "18:00"
    }

    "cut on the next day after 2nd limit" in {
      doubleCutOff.nextAvailableMoment("12:01", sameDay, nextDay) shouldBe
        "2019-03-20" :- "15:00"
    }
  }

  implicit private def toLocalTime(s: String): LocalTime = s.toLocalTime
  implicit private def toLocalDate(s: String): LocalDate = s.toLocalDate
}
