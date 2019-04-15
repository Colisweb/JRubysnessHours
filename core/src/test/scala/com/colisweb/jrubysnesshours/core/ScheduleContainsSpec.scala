package com.colisweb.jrubysnesshours.core

import java.time.ZonedDateTime

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleContainsSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  import Generators._
  import org.scalacheck.ops._

  "Schedule#contains" when {
    "the Schedule has an empty planning" should {
      val planningGen = Gen.listOfN(0, genTimeIntervalForWeekDay)

      def test(exceptionsGen: Gen[DateTimeInterval] => Gen[List[DateTimeInterval]]): Assertion =
        forAll(
          planningGen,
          exceptionsGen(genDateTimeInterval),
          genScheduleConstructor,
          Arbitrary.arbitrary[ZonedDateTime]
        ) {
          case (emptyPlanning, exceptions, scheduleContructor, date: ZonedDateTime) =>
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
        val exceptionsGen = Gen.listOfN(0, genDateTimeInterval)
        "an interval of the planning contains the date" should {
          "always be true" in {
            forAll(
              exceptionsGen,
              genScheduleConstructorContainingGeneratedZonedDateTime
            ) { (noExceptions, scheduleContructor) =>
              val (schedule: Schedule, date: ZonedDateTime) = scheduleContructor(noExceptions)

              schedule contains date shouldBe true
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

}
