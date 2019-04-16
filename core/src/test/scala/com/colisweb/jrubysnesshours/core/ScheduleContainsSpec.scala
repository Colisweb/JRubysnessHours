package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.{LocalTime, ZoneId, ZonedDateTime}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduleContainsSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  import Generators._
  import org.scalacheck.ops._

  "Schedule#contains" when {
    "With Scalacheck" when {
      "the Schedule has an empty planning" should {
        val withNoPlanning = genScheduler.map(_.copy(planning = Map.empty))
        "with no exceptions" should {
          val gen = withNoPlanning.map(_.copy(exceptions = Map.empty))
          "always be false" in forAll(gen, Arbitrary.arbitrary[ZonedDateTime]) { (schedule, date: ZonedDateTime) =>
            schedule contains date shouldBe false
          }
        }
        "with some exceptions" should {
          "always be false" in forAll(withNoPlanning, Arbitrary.arbitrary[ZonedDateTime]) {
            (schedule, date: ZonedDateTime) =>
              schedule contains date shouldBe false
          }
        }
      }
      "the Schedule has non empty planning" should {
        "with no exceptions" when {
          val withNoException = genScheduler.map(_.copy(exceptions = Map.empty))
          "an interval of the planning contains the date" should {
            val gen = (withNoException, Arbitrary.arbitrary[ZonedDateTime]).flatMap2(addPlanningEntryContaining)
            "always be true" in forAll(gen) {
              case (schedule, date) => schedule contains date shouldBe true
            }
          }
          "no interval of the planning contains the date" should {
            val gen = (withNoException, Arbitrary.arbitrary[ZonedDateTime]).flatMap2(removePlanningEntryContaining)
            "always be false" in forAll(gen) {
              case (schedule, date) => schedule contains date shouldBe false
            }
          }
        }
        "with some exceptions" should {
          "an interval of the planning contains the date" when {
            val withPlanningEntryContaining =
              (genScheduler, Arbitrary.arbitrary[ZonedDateTime]).flatMap2(addPlanningEntryContaining)

            "an exception also contains the date" should {
              val andWithAnExceptionContaining = withPlanningEntryContaining.flatMapT(addExceptionContaining)

              "be false" in forAll(andWithAnExceptionContaining) {
                case (schedule, date) => schedule contains date shouldBe false
              }
            }
            "no exception contains the date" should {
              val gen = withPlanningEntryContaining.mapT(removeExceptionsContaining)

              "be true" in forAll(gen) {
                case (schedule, date) => schedule contains date shouldBe true
              }
            }
          }
          "no interval of the planning contains the date" should {
            val gen = (genScheduler, Arbitrary.arbitrary[ZonedDateTime]).map2(removePlanningEntryContaining)

            "always be false" in forAll(gen) {
              case (schedule, date) => schedule contains date shouldBe false
            }
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

  private def addPlanningEntryContaining(schedule: Schedule, date: ZonedDateTime): Gen[(Schedule, ZonedDateTime)] =
    for {
      intervalSurounding <- genPlanningEntrySurrounding(date, schedule.timeZone)
    } yield schedule.copy(planning = schedule.planning ++ intervalSurounding) -> date

  private def addExceptionContaining(schedule: Schedule, date: ZonedDateTime): Gen[(Schedule, ZonedDateTime)] =
    for {
      exceptionContaining <- genExceptionSurronding(date, schedule.timeZone)
    } yield schedule.copy(exceptions = schedule.exceptions ++ exceptionContaining) -> date

  private def removePlanningEntryContaining(schedule: Schedule, date: ZonedDateTime): (Schedule, ZonedDateTime) = {
    val datetimeWithSameZone = date.withZoneSameInstant(schedule.timeZone)
    val time                 = datetimeWithSameZone.toLocalTime
    val dow                  = datetimeWithSameZone.getDayOfWeek
    val newDowInvervals =
      schedule.planning
        .getOrElse(dow, List.empty)
        .filterNot(_.contains(time))

    schedule.copy(planning = schedule.planning + (dow -> newDowInvervals)) -> date
  }

  private def removeExceptionsContaining(schedule: Schedule, date: ZonedDateTime): (Schedule, ZonedDateTime) = {
    val correctDate = date.withZoneSameInstant(schedule.timeZone)
    val localTime   = correctDate.toLocalTime
    val localDate   = correctDate.toLocalDate

    val newExceptionIntervals =
      schedule.exceptions
        .getOrElse(localDate, List.empty)
        .filterNot(_.contains(localTime))

    schedule.copy(exceptions = schedule.exceptions + (localDate -> newExceptionIntervals)) -> date
  }

}
