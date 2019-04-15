package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, ZonedDateTime}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleContainsSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  import Generators._
  import org.scalacheck.ops._

  "Schedule#contains" when {
    "the Schedule has an empty planning" should {
      val planningGen = genPlannings(0)

      def test(exceptionsGen: Gen[Map[LocalDate, List[TimeInterval]]]): Assertion =
        forAll(planningGen, exceptionsGen, genScheduleConstructor, Arbitrary.arbitrary[ZonedDateTime]) {
          case (emptyPlanning, noExceptions, scheduleContructor, date: ZonedDateTime) =>
            scheduleContructor(emptyPlanning, noExceptions) contains date shouldBe false
        }
      "with no exceptions" should {
        "always be false" in {
          test(genExceptions(0))
        }
      }
      "with some exceptions" should {
        "always be false" in {
          test(genNonEmptyExceptions)
        }
      }
    }
    "the Schedule has non empty planning" should {
      //def schedule(exceptionSize: Int): Gen[Schedule] = Gen.posNum[Int].flatMap(genSchedule(_, exceptionSize))

      "with no exceptions" should {}
      "with some exceptions" should {}
    }
  }

}
