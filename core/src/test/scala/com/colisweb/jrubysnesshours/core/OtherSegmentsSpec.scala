package com.colisweb.jrubysnesshours.core

import java.time.LocalDate

import com.colisweb.jrubysnesshours.core.Segments.excludingSegmentFromAnother
import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}

class OtherSegmentsSpec extends WordSpec with Matchers {

  "Merging mergeTimeSegments" should {
    val day = "2019-04-08"

    "Some overlapping TimeSegments ( for the same day ) with method merge1" in {

      val timeSegments =
        List(
          day at "02:00" - "05:00",
          day at "04:00" - "08:00",
          day at "09:00" - "19:00",
          day at "13:00" - "16:00",
        )

      val expected =
        List(
          day at "02:00" - "08:00",
          day at "09:00" - "19:00"
        )

      Segments.mergeSegments(timeSegments) should contain theSameElementsInOrderAs expected
      Segments.mergeSegments2(timeSegments) should contain theSameElementsInOrderAs expected

    }
  }

  "Excluding a timeSegment from another" when {
    val day = "2019-04-08"

    // TODO : generators with random could be great to check that the last case never append

    "05:00 -> 10:00" when {
      val includingSeg = day at "05:00" - "10:00"

      "excluding 04:00 -> 11:00 is empty" in {
        excludingSegmentFromAnother(includingSeg, day at "04:00" - "11:00") shouldBe empty
      }

      "excluding 05:00 -> 10:00 is empty" in {
        excludingSegmentFromAnother(includingSeg, day at "05:00" - "10:00") shouldBe empty
      }

      "excluding 01:00 -> 04:00 is 01:00 -> 04:00" in {
        excludingSegmentFromAnother(includingSeg, day at "01:00" - "04:00") shouldBe List(
          includingSeg
        )
      }

      "excluding 06:00 -> 11:00 is 05:00 -> 06:00" in {
        excludingSegmentFromAnother(includingSeg, day at "06:00" - "11:00") shouldBe List(
          day at "05:00" - "06:00"
        )
      }

      "excluding 06:00 -> 10:00 is 05:00 -> 06:00" in {
        excludingSegmentFromAnother(includingSeg, day at "06:00" - "10:00") shouldBe List(
          day at "05:00" - "06:00"
        )
      }

      "excluding 04:00 -> 09:00 is 09:00 -> 10:00" in {
        excludingSegmentFromAnother(includingSeg, day at "04:00" - "09:00") shouldBe List(
          day at "09:00" - "10:00"
        )
      }

      "excluding 05:00 -> 09:00 is 09:00 -> 10:00" in {
        excludingSegmentFromAnother(includingSeg, day at "05:00" - "09:00") shouldBe List(
          day at "09:00" - "10:00"
        )
      }

      "excluding is 06:00 -> 08:00 is 2 segments" in {
        excludingSegmentFromAnother(includingSeg, day at "06:00" - "08:00") shouldBe List(
          day at "05:00" - "06:00",
          day at "08:00" - "10:00"
        )
      }
    }

    "01:00 -> 04:00" when {
      val includingSeg = day at "01:00" - "04:00"

      "excluding 05:00 -> 10:00 is 01:00 -> 04:00" in {
        excludingSegmentFromAnother(includingSeg, day at "05:00" - "10:00") shouldBe List(
          includingSeg
        )
      }

      "excluding 04:00 -> 10:00 is 01:00 -> 04:00" in {
        excludingSegmentFromAnother(includingSeg, day at "04:00" - "10:00") shouldBe List(
          includingSeg
        )
      }
    }

    "Will return including start -> excluding start segment" should {
      "including is 05:00 -> 10:00 and excluding is 06:00 -> 11:00" in {

        val includingInterval = "05:00"- "10:00"
        val excludingInterval = "06:00"- "11:00"
        val date              = day

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-04-08", "05:00", "06:00")
        )
      }


    }
}
