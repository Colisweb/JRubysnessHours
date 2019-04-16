package com.colisweb.jrubysnesshours.core

import java.time.{LocalTime, ZoneId, ZonedDateTime}

import org.scalacheck.Gen
import org.scalatest.{Assertion, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import java.time.DayOfWeek._

class ScheduleContainsSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  import Generators._

  "Schedule#contains" when {
    "With Scalacheck" when {
      "the Schedule has an empty planning" should {
        val planningGen = Gen.listOfN(0, genTimeIntervalForWeekDay)

        def test(exceptionsGen: Gen[DateTimeInterval] => Gen[List[DateTimeInterval]]): Assertion =
          forAll(
            planningGen,
            exceptionsGen(genDateTimeInterval),
            genScheduleConstructor,
            genBoundedZonedDateTime
          ) { (emptyPlanning, exceptions, scheduleContructor, date: ZonedDateTime) =>
            scheduleContructor(emptyPlanning, exceptions) contains date shouldBe false
          }
        "with no exceptions" should {
          "always be false" in {
            test(Gen.listOfN(0, _))
          }
        }
        "with some exceptions" should {
          "always be false" in {
            test(Gen.nonEmptyListOf(_))
          }
        }
      }
      "the Schedule has non empty planning" should {
        "with no exceptions" when {
          "an interval of the planning contains the date" should {
            "always be true" in {
              forAll(genScheduleConstructorContainingGeneratedZonedDateTime) {
                case (schedule: Schedule, date: ZonedDateTime) => schedule contains date shouldBe true
              }
            }
          }
          "no interval of the planning contains the date" should {
            "always be false" in {}
          }
        }
        "with some exceptions" should {
          "an interval of the planning contains the date" when {
            "an exception also contains the date" should {
              "be false" in {}
            }
            "no exception contains the date" should {
              "be true" in {}
            }
          }
          "no interval of the planning contains the date" should {
            "always be false" in {}
          }
        }
      }
    }
    "with manual tests" when {
      "[Bug found by Scalacheck] The interval is very small but contains the date" should {
        "be true" in {
          val schedule =
            Schedule(
              planning = TimeIntervalForWeekDay(
                THURSDAY,
                TimeInterval(start = LocalTime.parse("18:59:59.999999999"), end = LocalTime.parse("19:00:00.000000001"))
              ) :: Nil,
              exceptions = Nil,
              timeZone = ZoneId.of("America/Sitka")
            )

          val date = ZonedDateTime.parse("1970-01-02T00:00-03:00[Etc/GMT+3]")

          schedule contains date shouldBe true
        }
      }
    }
  }

}
