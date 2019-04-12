package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time._

import org.scalatest.{Matchers, WordSpec}

class IntervalsSpec extends WordSpec with Matchers {

  import SpecUtils._

  val planning: Map[DayOfWeek, List[TimeInterval]] = Map(
    MONDAY -> List(TimeInterval("09:00".toLocalTime, "19:00".toLocalTime)),
    TUESDAY -> List(
      TimeInterval("09:30".toLocalTime, "14:00".toLocalTime),
      TimeInterval("15:00".toLocalTime, "19:00".toLocalTime)
    ),
    WEDNESDAY -> List(TimeInterval("09:30".toLocalTime, "20:00".toLocalTime)),
    THURSDAY  -> List(TimeInterval("09:30".toLocalTime, "19:00".toLocalTime)),
    FRIDAY    -> List(TimeInterval("09:30".toLocalTime, "19:00".toLocalTime)),
    SATURDAY -> List(
      TimeInterval("09:00".toLocalTime, "14:00".toLocalTime),
      TimeInterval("15:00".toLocalTime, "19:00".toLocalTime)
    )
  )
  val zoneId: ZoneId = ZoneId.of("Europe/Paris")

//
//
//  "Merging mergeTimeSegments" should {
//
//    "Some overlapping TimeSegments ( for the same day ) with method merge1" in {
//
//      val timeSegments =
//        List(
//          aTimeIntervalForDate("2019-04-08", "02:00", "05:00"),
//          aTimeIntervalForDate("2019-04-08", "04:00", "08:00"),
//          aTimeIntervalForDate("2019-04-08", "09:00", "19:00"),
//          aTimeIntervalForDate("2019-04-08", "13:00", "16:00"),
//        )
//
//      val expected =
//        List(
//          aTimeIntervalForDate("2019-04-08", "02:00", "08:00"),
//          aTimeIntervalForDate("2019-04-08", "09:00", "19:00")
//        )
//
//      Segments.mergeSegments(timeSegments) shouldBe expected
//      Segments.mergeSegments2(timeSegments) shouldBe expected
//
//    }
//  }
//
  "Apply exceptions to Interval" should {

    // TODO : generators with random could be great to check that the last case never append

    "Will return Nil" should {
      "including is empty and excluding is 04:00 -> 11:00" in {
        val excludingInterval = "04:00" - "11:00"

        val res = Schedule.cutExceptions(Nil, List(excludingInterval))

        res should contain theSameElementsInOrderAs Nil
      }

      "including is 05:00 -> 10:00 and excluding is 04:00 -> 11:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "04:00" - "11:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res shouldBe Nil
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 10:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "05:00" - "10:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res shouldBe Nil
      }
    }

    "Will return List(includingInterval)" should {
      "including is 05:00 -> 10:00 and excluding is 01:00 -> 04:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "01:00" - "04:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(includingInterval)
      }

      "including is 01:00 -> 04:00 and excluding is 05:00 -> 10:00" in {
        val includingInterval = "01:00" - "04:00"
        val excludingInterval = "05:00" - "10:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(includingInterval)
      }

      "including is 01:00 -> 04:00 and excluding is 04:00 -> 10:00" in {
        val includingInterval = "01:00" - "04:00"
        val excludingInterval = "04:00" - "10:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(includingInterval)
      }
    }

    "Will return including start -> excluding start segment" should {
      "including is 05:00 -> 10:00 and excluding is 06:00 -> 11:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "06:00" - "11:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List("05:00" - "06:00")
      }

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 10:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "06:00" - "10:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List("05:00" - "06:00")
      }
    }

    "Will return excluding end -> including end interval" should {
      "including is 05:00 -> 10:00 and excluding is 04:00 -> 09:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "04:00" - "09:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List("09:00" - "10:00")
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 09:00" in {
        val includingInterval = "05:00" - "10:00"
        val excludingInterval = "05:00" - "09:00"

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List("09:00" - "10:00")
      }
    }

    "Will return multiple intervals : (including.start -> excluding.start), (excluding.end -> including.end)" should {

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 08:00" in {
        val includingInterval = "05:00" - "10:00"

        val res = Schedule.cutExceptions(List(includingInterval), List("06:00" - "08:00"))

        res should contain theSameElementsInOrderAs List(
          "05:00" - "06:00",
          "08:00" - "10:00"
        )
      }

      "Will return 3 intervals " in {
        val includingInterval = "05:00" - "20:00"

        val exceptions =
          List(
            "06:00" - "08:00",
            "09:30" - "16:00",
            "19:00" - "22:00"
          )

        val res = Schedule.cutExceptions(List(includingInterval), exceptions)

        res should contain theSameElementsInOrderAs List(
          "05:00" - "06:00",
          "08:00" - "09:30",
          "16:00" - "19:00"
        )
      }

      "Will return 4 intervals " in {
        val includingInterval = "05:00" - "20:00"

        val exceptions =
          List(
            "06:00" - "08:00",
            "09:30" - "16:00",
            "18:00" - "19:00"
          )

        val res = Schedule.cutExceptions(List(includingInterval), exceptions)

        res should contain theSameElementsInOrderAs List(
          "05:00" - "06:00",
          "08:00" - "09:30",
          "16:00" - "18:00",
          "19:00" - "20:00"
        )
      }

    }
  }
}
