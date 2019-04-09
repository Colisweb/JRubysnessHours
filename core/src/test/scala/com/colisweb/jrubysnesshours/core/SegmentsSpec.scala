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

    val segmentsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeSegment] =
      Segments.segmentsBetween(planning, zoneId)

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
}
