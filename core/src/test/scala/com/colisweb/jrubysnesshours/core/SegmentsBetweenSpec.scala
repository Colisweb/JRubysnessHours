package com.colisweb.jrubysnesshours.core

import java.time._

import com.colisweb.jrubysnesshours.core.Core.TimeSegment
import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalatest.{Matchers, WordSpec}

class SegmentsBetweenSpec extends WordSpec with Matchers {

  "segments" when {

    "without exception" should {

      val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
        Segments.segmentsBetween(planning, zoneId, exceptions)

      "compute 2 segments between Thursday 18:00 to Friday 10:00" in {
        val segments =
          segmentsBetween("2019-03-21" at "18:00", "2019-03-22" at "10:00")
        segments should contain theSameElementsInOrderAs List(
          "2019-03-21" at "18:00" - "19:00",
          "2019-03-22" at "09:30" - "10:00"
        )
      }
      "compute 2 segments between Saturday 13:00 to same Saturday 16:00" in {
        val segments =
          segmentsBetween("2019-03-23" at "13:00", "2019-03-23" at "16:00")
        segments should contain theSameElementsInOrderAs List(
          "2019-03-23" at "13:00" - "14:00",
          "2019-03-23" at "15:00" - "16:00"
        )
      }
      "compute 3 segments between Saturday 13:00 to Monday 10:00" in {
        val segments =
          segmentsBetween("2019-03-23" at "13:00", "2019-03-25" at "10:00")
        segments should contain theSameElementsInOrderAs List(
          "2019-03-23" at "13:00" - "14:00",
          "2019-03-23" at "15:00" - "19:00",
          "2019-03-25" at "09:00" - "10:00"
        )
      }
      "compute 5 segments betweenSaturday 13:00 to Tuesday 16:00" in {
        val segments =
          segmentsBetween("2019-03-23" at "13:00", "2019-03-26" at "16:00")
        segments should contain theSameElementsInOrderAs List(
          "2019-03-23" at "13:00" - "14:00",
          "2019-03-23" at "15:00" - "19:00",
          "2019-03-25" at "09:00" - "19:00",
          "2019-03-26" at "09:30" - "14:00",
          "2019-03-26" at "15:00" - "16:00"
        )
      }
      "compute 2 segments between Sunday 13:00 to Tuesday 10:00" in {
        val segments =
          segmentsBetween("2019-03-24" at "13:00", "2019-03-26" at "10:00")
        segments should contain theSameElementsInOrderAs List(
          "2019-03-25" at "09:00" - "19:00",
          "2019-03-26" at "09:30" - "10:00"
        )
      }
      "compute 8 segments between Monday 09:00 to Sunday 23:00" in {
        val segments =
          segmentsBetween("2019-03-18" at "09:00", "2019-03-24" at "23:00")
        segments should contain theSameElementsInOrderAs List(
          "2019-03-18" at "09:00" - "19:00",
          "2019-03-19" at "09:30" - "14:00",
          "2019-03-19" at "15:00" - "19:00",
          "2019-03-20" at "09:30" - "20:00",
          "2019-03-21" at "09:30" - "19:00",
          "2019-03-22" at "09:30" - "19:00",
          "2019-03-23" at "09:00" - "14:00",
          "2019-03-23" at "15:00" - "19:00"
        )
      }
      "compute segments in April between Friday 13:40 to Tuesday 13:40" in {
        val segments =
          segmentsBetween("2019-04-05" at "13:40", "2019-04-09" at "13:40")
        segments should contain theSameElementsInOrderAs List(
          "2019-04-05" at "13:40" - "19:00",
          "2019-04-06" at "09:00" - "14:00",
          "2019-04-06" at "15:00" - "19:00",
          "2019-04-08" at "09:00" - "13:00",
          "2019-04-08" at "16:00" - "19:00",
          "2019-04-09" at "09:30" - "13:40"
        )
      }
    }

    "With exception" when {

      "the exception is not in the first or last day" should {
        val exceptionSegments = List(
          "2019-03-18" at "13:00" - "16:00"
        )

        val segmentsBetween
          : (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 6 segments between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the 2019-03-18 between 13:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              "2019-03-15" at "13:40",
              "2019-03-19" at "13:40"
            )
          s1 should contain theSameElementsInOrderAs List(
            "2019-03-15" at "13:40" - "19:00",
            "2019-03-16" at "09:00" - "14:00",
            "2019-03-16" at "15:00" - "19:00",
            "2019-03-18" at "09:00" - "13:00",
            "2019-03-18" at "16:00" - "19:00",
            "2019-03-19" at "09:30" - "13:40",
          )
        }
      }

      "the exception is during the first day" should {
        val exceptionSegments = List(
          "2019-03-15" at "13:00" - "16:00"
        )

        val segmentsBetween
          : (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 6 segments between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the  019-03-15 between 13:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              "2019-03-15" at "13:40",
              "2019-03-19" at "13:40"
            )
          s1 should contain theSameElementsInOrderAs List(
            "2019-03-15" at "16:00" - "19:00",
            "2019-03-16" at "09:00" - "14:00",
            "2019-03-16" at "15:00" - "19:00",
            "2019-03-18" at "09:00" - "19:00",
            "2019-03-19" at "09:30" - "13:40",
          )
        }
      }

      "the exception is during the last day" when {
        val exceptionSegments = List(
          "2019-03-19" at "13:00" - "16:00"
        )

        val segmentsBetween
          : (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 6 segments between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the 2019-03-19 between 13:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              "2019-03-15" at "13:40",
              "2019-03-19" at "13:40"
            )
          s1 should contain theSameElementsInOrderAs List(
            "2019-03-15" at "13:40" - "19:00",
            "2019-03-16" at "09:00" - "14:00",
            "2019-03-16" at "15:00" - "19:00",
            "2019-03-18" at "09:00" - "19:00",
            "2019-03-19" at "09:30" - "13:00"
          )
        }
      }

      "between a start and end the same day" should {
        val exceptionSegments = List(
          "2019-03-15" at "14:00" - "16:00"
        )

        val segmentsBetween
          : (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 2 segments between Friday 15 13:40 to Friday 15 19:00 INCLUDING and exception the 2019-03-18 between 14:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              "2019-03-15" at "13:40",
              "2019-03-15" at "19:00"
            )
          s1 should contain theSameElementsInOrderAs List(
            "2019-03-15" at "13:40" - "14:00",
            "2019-03-15" at "16:00" - "19:00",
          )
        }
      }

    }

  }
}