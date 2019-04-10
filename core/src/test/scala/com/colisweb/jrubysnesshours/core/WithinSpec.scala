package com.colisweb.jrubysnesshours.core

import java.time.{Duration, ZonedDateTime}
import scala.concurrent.duration._

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}

class WithinSpec extends WordSpec with Matchers {

  "within" should {

    val within: (ZonedDateTime, ZonedDateTime) => Duration =
      Core.within(planning, zoneId, Nil)

    "compute duration between Thursday 18:00 to Friday 10:00" in {
      val d1 = within(
        "2019-03-21" at "18:00",
        "2019-03-22" at "10:00"
      )
      d1 shouldBe aDuration(1, 30)
    }
    "compute 2 segments between Saturday 13:00 to same Saturday 16:00" in {
      val d2 =
        within(
          "2019-03-23" at "13:00",
          "2019-03-23" at "16:00"
        )
      d2 shouldBe aDuration(2)
    }
    "compute 3 segments between Saturday 13:00 to Monday 10:00" in {
      val d3 =
        within(
          "2019-03-23" at "13:00",
          "2019-03-25" at "10:00"
        )
      d3 shouldBe aDuration(6)
    }
    "compute 5 segments betweenSaturday 13:00 to Tuesday 16:00" in {
      val d4 =
        within(
          "2019-03-23" at "13:00",
          "2019-03-26" at "16:00"
        )
      d4 shouldBe aDuration(20, 30)
    }
    "compute 2 segments between Sunday 13:00 to Tuesday 10:00" in {
      val d5 =
        within(
          "2019-03-24" at "13:00",
          "2019-03-26" at "10:00"
        )
      d5 shouldBe aDuration(10, 30)
    }
    "compute 8 segments between Monday 09:00 to Sunday 23:00" in {
      val d6 =
        within(
          "2019-03-18" at "09:00",
          "2019-03-24" at "23:00"
        )
      d6 shouldBe aDuration(2, 9, 0)
    }
  }

}
