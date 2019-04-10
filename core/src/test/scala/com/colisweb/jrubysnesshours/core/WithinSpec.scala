package com.colisweb.jrubysnesshours.core

import java.time.{Duration, ZonedDateTime}

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}

class WithinSpec extends WordSpec with Matchers {

  "within" should {

    val within: (ZonedDateTime, ZonedDateTime) => Duration =
      Core.within(planning, zoneId, Nil)

    "compute duration between Thursday 18:00 to Friday 10:00" in {
      within("2019-03-21" at "18:00", "2019-03-22" at "10:00") shouldBe aDuration(
        hours = 1,
        minutes = 30
      )
    }

    "compute 2 segments between Saturday 13:00 to same Saturday 16:00" in {
      within("2019-03-23" at "13:00", "2019-03-23" at "16:00") shouldBe aDuration(
        hours = 2
      )
    }

    "compute 3 segments between Saturday 13:00 to Monday 10:00" in {
      within("2019-03-23" at "13:00", "2019-03-25" at "10:00") shouldBe aDuration(
        hours = 6
      )
    }

    "compute 5 segments between Saturday 13:00 to Tuesday 16:00" in {
      within("2019-03-23" at "13:00", "2019-03-26" at "16:00") shouldBe aDuration(
        hours = 20,
        minutes = 30
      )
    }

    "compute 2 segments between Sunday 13:00 to Tuesday 10:00" in {
      within("2019-03-24" at "13:00", "2019-03-26" at "10:00") shouldBe aDuration(
        hours = 10,
        minutes = 30
      )
    }

    "compute 8 segments between Monday 09:00 to Sunday 23:00" in {
      within("2019-03-18" at "09:00", "2019-03-24" at "23:00") shouldBe aDuration(
        days = 2,
        hours = 9,
        minutes = 0
      )
    }

    "compute 90 minutes between Friday 09:00 to 11:00" in {
      within("2019-04-05" at "09:00", "2019-04-05" at "11:00") shouldBe aDuration(
        hours = 1,
        minutes = 30
      )
    }

    "compute 120 minutes between Saturday 09:00 to 11:00" in {
      within("2019-04-06" at "09:00", "2019-04-06" at "11:00") shouldBe aDuration(
        hours = 2
      )
    }
  }

}
