package com.colisweb.jrubysnesshours.core

import org.scalatest.{Matchers, WordSpec}

class CutOffSpec extends WordSpec with Matchers {

  import SpecUtils._

  "DoubleCutOff" should {
    val doubleCutOff = DoubleCutOff(
      sameDay = CutOff(limit = "08:00".toLocalTime, firstAvailableTime = "18:00".toLocalTime),
      nextDay = CutOff(limit = "12:00".toLocalTime, firstAvailableTime = "15:00".toLocalTime)
    )

    "not cut before first limit" in {
      doubleCutOff.nextAvailableMoment("2019-05-02" :- "06:59") shouldBe "2019-05-02" :- "06:59"
    }

    "cut on the same day between 2 limits" in {
      doubleCutOff.nextAvailableMoment("2019-05-02" :- "08:01") shouldBe "2019-05-02" :- "18:00"
    }

    "cut on the next day after 2nd limit" in {
      doubleCutOff.nextAvailableMoment("2019-05-02" :- "12:01") shouldBe "2019-05-03" :- "15:00"
    }
  }
}
