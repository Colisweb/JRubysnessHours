package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek
import java.time.DayOfWeek._

import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleContainsForStartAndEndSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  "Schedule#contains for a start and a end ZonedDateTime" when {

    "the Schedule has an empty planning" should {

      val emptySchedule = schedule.copy(planning = Map.empty)

      "for the 2019-05-02 at 09:00 - 11:00" should {
        "for utc timezone" in {
          emptySchedule contains ("2019-05-02" at "09:00" -> "UTC", "2019-05-02" at "11:00" -> "UTC") shouldBe false
        }
        "for gmt+2 timezone" in {
          emptySchedule contains ("2019-05-02" at "09:00" -> "+02:00", "2019-05-02" at "11:00" -> "+02:00") shouldBe false
        }
        "for gmt+9 timezone" in {
          emptySchedule contains ("2019-05-02" at "09:00" -> "+09:00", "2019-05-02" at "11:00" -> "+09:00") shouldBe false
        }
        "for gmt-2 timezone" in {
          emptySchedule contains ("2019-05-02" at "09:00" -> "-02:00", "2019-05-02" at "11:00" -> "-02:00") shouldBe false
        }
      }

      "for the 2019-05-02 at 07:00 -- 09:00" should {
        "for utc timezone" in {
          emptySchedule contains ("2019-05-02" at "07:00" -> "UTC", "2019-05-02" at "09:00" -> "UTC") shouldBe false
        }
        "for gmt+2 timezone" in {
          emptySchedule contains ("2019-05-02" at "07:00" -> "+02:00", "2019-05-02" at "09:00" -> "+02:00") shouldBe false
        }
        "for gmt+9 timezone" in {
          emptySchedule contains ("2019-05-02" at "07:00" -> "+09:00", "2019-05-02" at "09:00" -> "+09:00") shouldBe false
        }
        "for gmt-2 timezone" in {
          emptySchedule contains ("2019-05-02" at "07:00" -> "-02:00", "2019-05-02" at "09:00" -> "-02:00") shouldBe false
        }
      }
    }

    "the Schedule has a planning for 9h -> 19h Europe/Paris" when {
      val planning: Map[DayOfWeek, List[TimeInterval]] = Map(
        MONDAY    -> List("09:00" - "19:00"),
        TUESDAY   -> List("09:00" - "19:00"),
        WEDNESDAY -> List("09:00" - "19:00"),
        THURSDAY  -> List("09:00" - "19:00"),
        FRIDAY    -> List("09:00" - "19:00"),
        SATURDAY  -> List("09:00" - "19:00")
      )

      "with no exception" when {
        val regularSchedule = schedule.copy(planning = planning)

        "for the 2019-05-02 at 09:00 - 11:00" should {
          "for utc timezone" in {
            regularSchedule contains ("2019-05-02" at "09:00" -> "UTC", "2019-05-02" at "11:00" -> "UTC") shouldBe true
          }
          "for gmt+2 timezone" in {
            regularSchedule contains ("2019-05-02" at "09:00" -> "+02:00", "2019-05-02" at "11:00" -> "+02:00") shouldBe true
          }
          "for gmt+9 timezone" in {
            regularSchedule contains ("2019-05-02" at "09:00" -> "+09:00", "2019-05-02" at "11:00" -> "+09:00") shouldBe false
          }
          "for gmt-2 timezone" in {
            regularSchedule contains ("2019-05-02" at "09:00" -> "-02:00", "2019-05-02" at "11:00" -> "-02:00") shouldBe true
          }
        }

        "for the 2019-05-02 at 07:00 -- 09:00" should {
          "for utc timezone" in {
            regularSchedule contains ("2019-05-02" at "07:00" -> "UTC", "2019-05-02" at "09:00" -> "UTC") shouldBe true
          }
          "for gmt+2 timezone" in {
            regularSchedule contains ("2019-05-02" at "07:00" -> "+02:00", "2019-05-02" at "09:00" -> "+02:00") shouldBe false
          }
          "for gmt+9 timezone" in {
            regularSchedule contains ("2019-05-02" at "07:00" -> "+09:00", "2019-05-02" at "09:00" -> "+09:00") shouldBe false
          }
          "for gmt-2 timezone" in {
            regularSchedule contains ("2019-05-02" at "07:00" -> "-02:00", "2019-05-02" at "09:00" -> "-02:00") shouldBe true
          }
        }
      }

      "with an exception for  2019-05-02 at 09:00 to 11:00 ( enclosing some shift )" when {
        val exception = "2019-05-02" at "09:00" - "11:00"
        val withExceptionSchedule =
          schedule.copy(planning = planning, exceptions = List(exception.date -> List(exception.interval)).toMap)

        "for the 2019-05-02 at 09:00 - 11:00" should {
          "for utc timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "UTC", "2019-05-02" at "11:00" -> "UTC") shouldBe true
          }
          "for gmt+2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "+02:00", "2019-05-02" at "11:00" -> "+02:00") shouldBe false
          }
          "for gmt+9 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "+09:00", "2019-05-02" at "11:00" -> "+09:00") shouldBe false
          }
          "for gmt-2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "-02:00", "2019-05-02" at "11:00" -> "-02:00") shouldBe true
          }
        }

        "for the 2019-05-02 at 07:00 -- 09:00" should {
          "for utc timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "UTC", "2019-05-02" at "09:00" -> "UTC") shouldBe false
          }
          "for gmt+2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "+02:00", "2019-05-02" at "09:00" -> "+02:00") shouldBe false
          }
          "for gmt+9 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "+09:00", "2019-05-02" at "09:00" -> "+09:00") shouldBe false
          }
          "for gmt-2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "-02:00", "2019-05-02" at "09:00" -> "-02:00") shouldBe true
          }
        }
      }

      "with an exception for  2019-05-02 at 10:00 to 10:30 (not enclosing but touch some shifts)" when {
        val exception = "2019-05-02" at "10:00" - "10:30"
        val withExceptionSchedule =
          schedule.copy(planning = planning, exceptions = List(exception.date -> List(exception.interval)).toMap)

        "for the 2019-05-02 at 09:00 - 11:00" should {
          "for utc timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "UTC", "2019-05-02" at "11:00" -> "UTC") shouldBe true
          }
          "for gmt+2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "+02:00", "2019-05-02" at "11:00" -> "+02:00") shouldBe false
          }
          "for gmt+9 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "+09:00", "2019-05-02" at "11:00" -> "+09:00") shouldBe false
          }
          "for gmt-2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "09:00" -> "-02:00", "2019-05-02" at "11:00" -> "-02:00") shouldBe true
          }
        }

        "for the 2019-05-02 at 07:00 -- 09:00" should {
          "for utc timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "UTC", "2019-05-02" at "09:00" -> "UTC") shouldBe false
          }
          "for gmt+2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "+02:00", "2019-05-02" at "09:00" -> "+02:00") shouldBe false
          }
          "for gmt+9 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "+09:00", "2019-05-02" at "09:00" -> "+09:00") shouldBe false
          }
          "for gmt-2 timezone" in {
            withExceptionSchedule contains ("2019-05-02" at "07:00" -> "-02:00", "2019-05-02" at "09:00" -> "-02:00") shouldBe true
          }
        }
      }
    }
  }
}
