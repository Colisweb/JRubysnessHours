package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time._

import org.scalatest.{Matchers, WordSpec}

class IntervalsSpec extends WordSpec with Matchers {

  implicit class StringToLocalTime(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)
  }

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

  "Intervals" should {

    "without exceptions" should {

      val schedule = Schedule.apply(planning, Map.empty[LocalDate, List[TimeInterval]], zoneId)
      val intervalsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeIntervalForDate] =
        schedule.intervalsBetween

      "compute 2 intervals between Thursday 18:00 to Friday 10:00" in {
        val s1 = intervalsBetween(
          parseZonedDateTime("2019-03-21", "18:00"),
          parseZonedDateTime("2019-03-22", "10:00")
        )
        s1 should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-03-21", "18:00", "19:00"),
          aTimeIntervalForDate("2019-03-22", "09:30", "10:00")
        )
      }
      "compute 2 intervals between Saturday 13:00 to same Saturday 16:00" in {
        val s2 =
          intervalsBetween(
            parseZonedDateTime("2019-03-23", "13:00"),
            parseZonedDateTime("2019-03-23", "16:00")
          )
        s2 should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-03-23", "13:00", "14:00"),
          aTimeIntervalForDate("2019-03-23", "15:00", "16:00")
        )
      }
      "compute 3 intervals between Saturday 13:00 to Monday 10:00" in {
        val s3 =
          intervalsBetween(
            parseZonedDateTime("2019-03-23", "13:00"),
            parseZonedDateTime("2019-03-25", "10:00")
          )
        s3 should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-03-23", "13:00", "14:00"),
          aTimeIntervalForDate("2019-03-23", "15:00", "19:00"),
          aTimeIntervalForDate("2019-03-25", "09:00", "10:00")
        )
      }
      "compute 5 intervals betweenSaturday 13:00 to Tuesday 16:00" in {
        val s4 =
          intervalsBetween(
            parseZonedDateTime("2019-03-23", "13:00"),
            parseZonedDateTime("2019-03-26", "16:00")
          )
        s4 should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-03-23", "13:00", "14:00"),
          aTimeIntervalForDate("2019-03-23", "15:00", "19:00"),
          aTimeIntervalForDate("2019-03-25", "09:00", "19:00"),
          aTimeIntervalForDate("2019-03-26", "09:30", "14:00"),
          aTimeIntervalForDate("2019-03-26", "15:00", "16:00")
        )
      }
      "compute 2 intervals between Sunday 13:00 to Tuesday 10:00" in {
        val s5 =
          intervalsBetween(
            parseZonedDateTime("2019-03-24", "13:00"),
            parseZonedDateTime("2019-03-26", "10:00")
          )
        s5 should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-03-25", "09:00", "19:00"),
          aTimeIntervalForDate("2019-03-26", "09:30", "10:00")
        )
      }
      "compute 8 intervals between Monday 09:00 to Sunday 23:00" in {
        val s6 =
          intervalsBetween(
            parseZonedDateTime("2019-03-18", "09:00"),
            parseZonedDateTime("2019-03-24", "23:00")
          )
        s6 should contain theSameElementsInOrderAs List(
          aTimeIntervalForDate("2019-03-18", "09:00", "19:00"),
          aTimeIntervalForDate("2019-03-19", "09:30", "14:00"),
          aTimeIntervalForDate("2019-03-19", "15:00", "19:00"),
          aTimeIntervalForDate("2019-03-20", "09:30", "20:00"),
          aTimeIntervalForDate("2019-03-21", "09:30", "19:00"),
          aTimeIntervalForDate("2019-03-22", "09:30", "19:00"),
          aTimeIntervalForDate("2019-03-23", "09:00", "14:00"),
          aTimeIntervalForDate("2019-03-23", "15:00", "19:00")
        )
      }
    }

    "with exceptions" should {

      "the exception is not in the first or last day" should {
        val exceptions = Map(
          parseDate("2019-03-18") -> List(
            parseInterval("13:00", "16:00")
          )
        )

        val schedule = Schedule(planning, exceptions, zoneId)
        val intervalsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeIntervalForDate] =
          schedule.intervalsBetween

        "compute 6 intervals between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the 2019-03-18 between 13:00 and 16:00" in {
          val s1 =
            intervalsBetween(
              parseZonedDateTime("2019-03-15", "13:40"),
              parseZonedDateTime("2019-03-19", "13:40")
            )
          s1 should contain theSameElementsInOrderAs List(
            aTimeIntervalForDate("2019-03-15", "13:40", "19:00"),
            aTimeIntervalForDate("2019-03-16", "09:00", "14:00"),
            aTimeIntervalForDate("2019-03-16", "15:00", "19:00"),
            aTimeIntervalForDate("2019-03-18", "09:00", "13:00"),
            aTimeIntervalForDate("2019-03-18", "16:00", "19:00"),
            aTimeIntervalForDate("2019-03-19", "09:30", "13:40")
          )
        }
      }

      "the exception is during the first day" should {
        val exceptions = Map(
          parseDate("2019-03-15") -> List(
            parseInterval("13:00", "16:00")
          )
        )

        val schedule = Schedule(planning, exceptions, zoneId)
        val intervalsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeIntervalForDate] =
          schedule.intervalsBetween

        "compute 6 intervals between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the  019-03-15 between 13:00 and 16:00" in {
          val s1 =
            intervalsBetween(
              parseZonedDateTime("2019-03-15", "13:40"),
              parseZonedDateTime("2019-03-19", "13:40")
            )
          s1 should contain theSameElementsInOrderAs List(
            aTimeIntervalForDate("2019-03-15", "16:00", "19:00"),
            aTimeIntervalForDate("2019-03-16", "09:00", "14:00"),
            aTimeIntervalForDate("2019-03-16", "15:00", "19:00"),
            aTimeIntervalForDate("2019-03-18", "09:00", "19:00"),
            aTimeIntervalForDate("2019-03-19", "09:30", "13:40")
          )
        }
      }

      "the exception is during the last day" should {
        val exceptions = Map(
          parseDate("2019-03-19") -> List(
            parseInterval("13:00", "16:00")
          )
        )

        val schedule = Schedule(planning, exceptions, zoneId)
        val intervalsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeIntervalForDate] =
          schedule.intervalsBetween

        "compute 6 intervals between Friday 15 13:40 to Tuesday 19 13:40 INCLUDING and exception the 2019-03-19 between 13:00 and 16:00" in {
          val s1 =
            intervalsBetween(
              parseZonedDateTime("2019-03-15", "13:40"),
              parseZonedDateTime("2019-03-19", "13:40")
            )
          s1 should contain theSameElementsInOrderAs List(
            aTimeIntervalForDate("2019-03-15", "13:40", "19:00"),
            aTimeIntervalForDate("2019-03-16", "09:00", "14:00"),
            aTimeIntervalForDate("2019-03-16", "15:00", "19:00"),
            aTimeIntervalForDate("2019-03-18", "09:00", "19:00"),
            aTimeIntervalForDate("2019-03-19", "09:30", "13:00")
          )
        }
      }

      "between a start and end the same day" should {
        val exceptions = Map(
          parseDate("2019-03-15") -> List(
            parseInterval("14:00", "16:00")
          )
        )

        val schedule = Schedule(planning, exceptions, zoneId)
        val intervalsBetween: (ZonedDateTime, ZonedDateTime) => List[TimeIntervalForDate] =
          schedule.intervalsBetween

        "compute 2 intervals between Friday 15 13:40 to Friday 15 19:00 INCLUDING and exception the 2019-03-18 between 14:00 and 16:00" in {
          val s1 =
            intervalsBetween(
              parseZonedDateTime("2019-03-15", "13:40"),
              parseZonedDateTime("2019-03-15", "19:00")
            )
          s1 should contain theSameElementsInOrderAs List(
            aTimeIntervalForDate("2019-03-15", "13:40", "14:00"),
            aTimeIntervalForDate("2019-03-15", "16:00", "19:00")
          )
        }
      }
    }
  }

  def parseZonedDateTime(day: String, time: String): ZonedDateTime =
    ZonedDateTime.parse(s"${day}T$time:00.000+01:00[$zoneId]")

  def parseInterval(startTime: String, endTime: String) =
    TimeInterval(
      LocalTime.parse(s"$startTime:00"),
      LocalTime.parse(s"$endTime:00")
    )

  def parseDate(date: String): LocalDate = LocalDate.parse(date)

  def aTimeIntervalForDate(date: String, startTime: String, endTime: String) =
    TimeIntervalForDate(
      parseDate(date),
      parseInterval(startTime, endTime)
    )
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
        val excludingInterval = parseInterval("04:00", "11:00")

        val res = Schedule.cutExceptions(Nil, List(excludingInterval))

        res should contain theSameElementsInOrderAs Nil
      }

      "including is 05:00 -> 10:00 and excluding is 04:00 -> 11:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("04:00", "11:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res shouldBe Nil
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 10:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("05:00", "10:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res shouldBe Nil
      }
    }

    "Will return List(includingInterval)" should {
      "including is 05:00 -> 10:00 and excluding is 01:00 -> 04:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("01:00", "04:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(includingInterval)
      }

      "including is 01:00 -> 04:00 and excluding is 05:00 -> 10:00" in {
        val includingInterval = parseInterval("01:00", "04:00")
        val excludingInterval = parseInterval("05:00", "10:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(includingInterval)
      }

      "including is 01:00 -> 04:00 and excluding is 04:00 -> 10:00" in {
        val includingInterval = parseInterval("01:00", "04:00")
        val excludingInterval = parseInterval("04:00", "10:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(includingInterval)
      }
    }

    "Will return including start -> excluding start segment" should {
      "including is 05:00 -> 10:00 and excluding is 06:00 -> 11:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("06:00", "11:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(parseInterval("05:00", "06:00"))
      }

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 10:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("06:00", "10:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(parseInterval("05:00", "06:00"))
      }
    }

    "Will return excluding end -> including end interval" should {
      "including is 05:00 -> 10:00 and excluding is 04:00 -> 09:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("04:00", "09:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(parseInterval("09:00", "10:00"))
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 09:00" in {
        val includingInterval = parseInterval("05:00", "10:00")
        val excludingInterval = parseInterval("05:00", "09:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(excludingInterval))

        res should contain theSameElementsInOrderAs List(parseInterval("09:00", "10:00"))
      }
    }

    "Will return multiple intervals : (including.start -> excluding.start), (excluding.end -> including.end)" should {

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 08:00" in {
        val includingInterval = parseInterval("05:00", "10:00")

        val res = Schedule.cutExceptions(List(includingInterval), List(parseInterval("06:00", "08:00")))

        res should contain theSameElementsInOrderAs List(
          parseInterval("05:00", "06:00"),
          parseInterval("08:00", "10:00")
        )
      }

      "Will return 3 intervals " in {
        val includingInterval = parseInterval("05:00", "20:00")

        val exceptions =
          List(
            parseInterval("06:00", "08:00"),
            parseInterval("09:30", "16:00"),
            parseInterval("19:00", "22:00")
          )

        val res = Schedule.cutExceptions(List(includingInterval), exceptions)

        res should contain theSameElementsInOrderAs List(
          parseInterval("05:00", "06:00"),
          parseInterval("08:00", "09:30"),
          parseInterval("16:00", "19:00")
        )
      }

      "Will return 4 intervals " in {
        val includingInterval = parseInterval("05:00", "20:00")

        val exceptions =
          List(
            parseInterval("06:00", "08:00"),
            parseInterval("09:30", "16:00"),
            parseInterval("18:00", "19:00")
          )

        val res = Schedule.cutExceptions(List(includingInterval), exceptions)

        res should contain theSameElementsInOrderAs List(
          parseInterval("05:00", "06:00"),
          parseInterval("08:00", "09:30"),
          parseInterval("16:00", "18:00"),
          parseInterval("19:00", "20:00")
        )
      }

    }
  }
}
