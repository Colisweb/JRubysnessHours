package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time._

import com.colisweb.jrubysnesshours.core.Core.{
  BusinessHoursByDayOfWeek,
  Interval,
  TimeSegment
}
import org.scalatest.{Matchers, WordSpec}

class SegmentsSpec extends WordSpec with Matchers {

  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)
  }

  val planning: BusinessHoursByDayOfWeek = Map(
    MONDAY -> List(Interval("09:00".toLocalTime, "19:00".toLocalTime)),
    TUESDAY -> List(
      Interval("09:30".toLocalTime, "14:00".toLocalTime),
      Interval("15:00".toLocalTime, "19:00".toLocalTime)
    ),
    WEDNESDAY -> List(Interval("09:30".toLocalTime, "20:00".toLocalTime)),
    THURSDAY -> List(Interval("09:30".toLocalTime, "19:00".toLocalTime)),
    FRIDAY -> List(Interval("09:30".toLocalTime, "19:00".toLocalTime)),
    SATURDAY -> List(
      Interval("09:00".toLocalTime, "14:00".toLocalTime),
      Interval("15:00".toLocalTime, "19:00".toLocalTime)
    )
  )
  val zoneId: ZoneId = ZoneId.of("Europe/Paris")

  "segments" should {

    "without exception" should {

      val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
        Segments.segmentsBetween(planning, zoneId, Nil)

      "compute 2 segments between Thursday 18:00 to Friday 10:00" in {
        val s1 = segmentsBetween(
          aDayAt("2019-03-21", "18:00"),
          aDayAt("2019-03-22", "10:00")
        )
        s1 should contain theSameElementsInOrderAs List(
          aSegment("2019-03-21", "18:00", "19:00"),
          aSegment("2019-03-22", "09:30", "10:00")
        )
      }
      "compute 2 segments between Saturday 13:00 to same Saturday 16:00" in {
        val s2 =
          segmentsBetween(
            aDayAt("2019-03-23", "13:00"),
            aDayAt("2019-03-23", "16:00")
          )
        s2 should contain theSameElementsInOrderAs List(
          aSegment("2019-03-23", "13:00", "14:00"),
          aSegment("2019-03-23", "15:00", "16:00")
        )
      }
      "compute 3 segments between Saturday 13:00 to Monday 10:00" in {
        val s3 =
          segmentsBetween(
            aDayAt("2019-03-23", "13:00"),
            aDayAt("2019-03-25", "10:00")
          )
        s3 should contain theSameElementsInOrderAs List(
          aSegment("2019-03-23", "13:00", "14:00"),
          aSegment("2019-03-23", "15:00", "19:00"),
          aSegment("2019-03-25", "09:00", "10:00")
        )
      }
      "compute 5 segments betweenSaturday 13:00 to Tuesday 16:00" in {
        val s4 =
          segmentsBetween(
            aDayAt("2019-03-23", "13:00"),
            aDayAt("2019-03-26", "16:00")
          )
        s4 should contain theSameElementsInOrderAs List(
          aSegment("2019-03-23", "13:00", "14:00"),
          aSegment("2019-03-23", "15:00", "19:00"),
          aSegment("2019-03-25", "09:00", "19:00"),
          aSegment("2019-03-26", "09:30", "14:00"),
          aSegment("2019-03-26", "15:00", "16:00")
        )
      }
      "compute 2 segments between Sunday 13:00 to Tuesday 10:00" in {
        val s5 =
          segmentsBetween(
            aDayAt("2019-03-24", "13:00"),
            aDayAt("2019-03-26", "10:00")
          )
        s5 should contain theSameElementsInOrderAs List(
          aSegment("2019-03-25", "09:00", "19:00"),
          aSegment("2019-03-26", "09:30", "10:00")
        )
      }
      "compute 8 segments between Monday 09:00 to Sunday 23:00" in {
        val s6 =
          segmentsBetween(
            aDayAt("2019-03-18", "09:00"),
            aDayAt("2019-03-24", "23:00")
          )
        s6 should contain theSameElementsInOrderAs List(
          aSegment("2019-03-18", "09:00", "19:00"),
          aSegment("2019-03-19", "09:30", "14:00"),
          aSegment("2019-03-19", "15:00", "19:00"),
          aSegment("2019-03-20", "09:30", "20:00"),
          aSegment("2019-03-21", "09:30", "19:00"),
          aSegment("2019-03-22", "09:30", "19:00"),
          aSegment("2019-03-23", "09:00", "14:00"),
          aSegment("2019-03-23", "15:00", "19:00")
        )
      }
    }


    "With exception" should {

      "the exception is not in the first or last day" should {
        val exceptionSegments = List(
          aSegment("2019-03-18", "13:00", "16:00")
        )

        val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 6 segments between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the 2019-03-18 between 13:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              aDayAt("2019-03-15", "13:40"),
              aDayAt("2019-03-19", "13:40")
            )
          s1 should contain theSameElementsInOrderAs List(
            aSegment("2019-03-15", "13:40", "19:00"),
            aSegment("2019-03-16", "09:00", "14:00"),
            aSegment("2019-03-16", "15:00", "19:00"),
            aSegment("2019-03-18", "09:00", "13:00"),
            aSegment("2019-03-18", "16:00", "19:00"),
            aSegment("2019-03-19", "09:30", "13:40"),
          )
        }
      }

      "the exception is during the first day" should {
        val exceptionSegments = List(
          aSegment("2019-03-15", "13:00", "16:00")
        )

        val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 6 segments between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the  019-03-15 between 13:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              aDayAt("2019-03-15", "13:40"),
              aDayAt("2019-03-19", "13:40")
            )
          s1 should contain theSameElementsInOrderAs List(
            aSegment("2019-03-15", "16:00", "19:00"),
            aSegment("2019-03-16", "09:00", "14:00"),
            aSegment("2019-03-16", "15:00", "19:00"),
            aSegment("2019-03-18", "09:00", "19:00"),
            aSegment("2019-03-19", "09:30", "13:40"),
          )
        }
      }

      "the exception is during the last day" should {
        val exceptionSegments = List(
          aSegment("2019-03-19", "13:00", "16:00")
        )

        val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 6 segments between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the 2019-03-19 between 13:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              aDayAt("2019-03-15", "13:40"),
              aDayAt("2019-03-19", "13:40")
            )
          s1 should contain theSameElementsInOrderAs List(
            aSegment("2019-03-15", "13:40", "19:00"),
            aSegment("2019-03-16", "09:00", "14:00"),
            aSegment("2019-03-16", "15:00", "19:00"),
            aSegment("2019-03-18", "09:00", "19:00"),
            aSegment("2019-03-19", "09:30", "13:00")
          )
        }
      }

      "between a start and end the same day" should {
        val exceptionSegments = List(
          aSegment("2019-03-15", "14:00", "16:00")
        )

        val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
          Segments.segmentsBetween(planning, zoneId, exceptionSegments)

        "compute 2 segments between Friday 15 13:40 to Friday 15 19:00 INCLUDING and exception the 2019-03-18 between 14:00 and 16:00" in {
          val s1 =
            segmentsBetween(
              aDayAt("2019-03-15", "13:40"),
              aDayAt("2019-03-15", "19:00")
            )
          s1 should contain theSameElementsInOrderAs List(
            aSegment("2019-03-15", "13:40", "14:00"),
            aSegment("2019-03-15", "16:00", "19:00"),
          )
        }
      }

    }


  }

  def aDayAt(day: String, time: String): ZonedDateTime =
    ZonedDateTime.parse(s"${day}T$time:00.000+01:00[$zoneId]")

  def aSegment(date: String, startTime: String, endTime: String) =
    TimeSegment(
      LocalDate.parse(date),
      Interval(
        LocalTime.parse(s"$startTime:00"),
        LocalTime.parse(s"$endTime:00")
      )
    )

  "Merging mergeTimeSegments" should {

    "Some overlapping TimeSegments ( for the same day ) with method merge1" in {

      val timeSegments =
        List(
          aSegment("2019-04-08", "02:00", "05:00"),
          aSegment("2019-04-08", "04:00", "08:00"),
          aSegment("2019-04-08", "09:00", "19:00"),
          aSegment("2019-04-08", "13:00", "16:00"),
        )

      val expected =
        List(
          aSegment("2019-04-08", "02:00", "08:00"),
          aSegment("2019-04-08", "09:00", "19:00")
        )

      Segments.mergeSegments(timeSegments) should contain theSameElementsInOrderAs expected
      Segments.mergeSegments2(timeSegments) should contain theSameElementsInOrderAs expected

    }
  }

  "Excluding a timeSegment from another" should {

    // TODO : generators with random could be great to check that the last case never append

    "Will return Nil" should {
      "including is 05:00 -> 10:00 and excluding is 04:00 -> 11:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "04:00", "11:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs Nil
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 10:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "05:00", "10:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs Nil
      }
    }

    "Will return List(includingSeg)" should {
      "including is 05:00 -> 10:00 and excluding is 01:00 -> 04:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "01:00", "04:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(includingSeg)
      }

      "including is 01:00 -> 04:00 and excluding is 05:00 -> 10:00" in {

        val includingSeg = aSegment("2019-04-08", "01:00", "04:00")
        val excludingSeg = aSegment("2019-04-08", "05:00", "10:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(includingSeg)
      }

      "including is 01:00 -> 04:00 and excluding is 04:00 -> 10:00" in {

        val includingSeg = aSegment("2019-04-08", "01:00", "04:00")
        val excludingSeg = aSegment("2019-04-08", "04:00", "10:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(includingSeg)
      }
    }

    "Will return including start -> excluding start segment" should {
      "including is 05:00 -> 10:00 and excluding is 06:00 -> 11:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "06:00", "11:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(
          aSegment("2019-04-08", "05:00", "06:00")
        )
      }

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 10:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "06:00", "10:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(
          aSegment("2019-04-08", "05:00", "06:00")
        )
      }
    }

    "Will return excluding end -> including end segment" should {
      "including is 05:00 -> 10:00 and excluding is 04:00 -> 09:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "04:00", "09:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(
          aSegment("2019-04-08", "09:00", "10:00")
        )
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 09:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "05:00", "09:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(
          aSegment("2019-04-08", "09:00", "10:00")
        )
      }
    }

    "Will return two segment : (including.start -> excluding.start), (excluding.end -> including.end)" should {
      "including is 05:00 -> 10:00 and excluding is 06:00 -> 08:00" in {

        val includingSeg = aSegment("2019-04-08", "05:00", "10:00")
        val excludingSeg = aSegment("2019-04-08", "06:00", "08:00")

        val res =
          Segments.excludingSegmentFromAnother(includingSeg, excludingSeg)
        res should contain theSameElementsInOrderAs List(
          aSegment("2019-04-08", "05:00", "06:00"),
          aSegment("2019-04-08", "08:00", "10:00")
        )
      }
    }
  }
}
