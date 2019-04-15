package com.colisweb.jrubysnesshours.core

import java.time.{Duration => _}

import SpecUtils._
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class ScheduleWithinSpec extends WordSpec with Matchers {

  "Schedule#within" should {
    "compute duration between Thursday 18:00 to Friday 10:00" in {
      schedule.within("2019-03-21" at "18:00", "2019-03-22" at "10:00") shouldBe 1.hours + 30.minutes
    }

    "compute 2 segments between Saturday 13:00 to same Saturday 16:00" in {
      schedule.within("2019-03-23" at "13:00", "2019-03-23" at "16:00") shouldBe 2.hours
    }

    "compute 3 segments between Saturday 13:00 to Monday 10:00" in {
      schedule.within("2019-03-23" at "13:00", "2019-03-25" at "10:00") shouldBe 6.hours
    }

    "compute 5 segments between Saturday 13:00 to Tuesday 16:00" in {
      schedule.within("2019-03-23" at "13:00", "2019-03-26" at "16:00") shouldBe 20.hours + 30.minutes
    }

    "compute 2 segments between Sunday 13:00 to Tuesday 10:00" in {
      schedule.within("2019-03-24" at "13:00", "2019-03-26" at "10:00") shouldBe 10.hours + 30.minutes
    }

    "compute 8 segments between Monday 09:00 to Sunday 23:00" in {
      schedule.within("2019-03-18" at "09:00", "2019-03-24" at "23:00") shouldBe 2.days + 9.hours
    }

    "compute 90 minutes between Friday 09:00 to 11:00" in {
      schedule.within("2019-04-05" at "09:00", "2019-04-05" at "11:00") shouldBe 1.hours + 30.minutes
    }

    "compute 120 minutes between Saturday 09:00 to 11:00" in {
      schedule.within("2019-04-06" at "09:00", "2019-04-06" at "11:00") shouldBe 2.hours
    }
  }

}
