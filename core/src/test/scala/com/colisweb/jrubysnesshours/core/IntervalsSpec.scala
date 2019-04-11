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

  def aDayAt(day: String, time: String): ZonedDateTime =
    ZonedDateTime.parse(s"${day}T$time:00.000+01:00[$zoneId]")

  def aTimeInterval(startTime: String, endTime: String) =
    TimeInterval(
      LocalTime.parse(s"$startTime:00"),
      LocalTime.parse(s"$endTime:00")
    )

  def aDate(date: String): LocalDate =
    LocalDate.parse(date)

  def aTimeIntervalForDate(date: String, startTime: String, endTime: String) =
    TimeIntervalForDate(
      aDate(date),
      aTimeInterval(startTime, endTime)
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
      "including is 05:00 -> 10:00 and excluding is 04:00 -> 11:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("04:00", "11:00")

        val date = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe Nil
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 10:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("05:00", "10:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe Nil
      }
    }

    "Will return List(includingInterval)" should {
      "including is 05:00 -> 10:00 and excluding is 01:00 -> 04:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("01:00", "04:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          TimeIntervalForDate(date, includingInterval)
        )
      }

      "including is 01:00 -> 04:00 and excluding is 05:00 -> 10:00" in {

        val includingInterval = aTimeInterval("01:00", "04:00")
        val excludingInterval = aTimeInterval("05:00", "10:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          TimeIntervalForDate(date, includingInterval)
        )
      }

      "including is 01:00 -> 04:00 and excluding is 04:00 -> 10:00" in {

        val includingInterval = aTimeInterval("01:00", "04:00")
        val excludingInterval = aTimeInterval("04:00", "10:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          TimeIntervalForDate(date, includingInterval)
        )
      }
    }

    "Will return including start -> excluding start segment" should {
      "including is 05:00 -> 10:00 and excluding is 06:00 -> 11:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("06:00", "11:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "05:00", "06:00")
        )
      }

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 10:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("06:00", "10:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "05:00", "06:00")
        )
      }
    }

    "Will return excluding end -> including end interval" should {
      "including is 05:00 -> 10:00 and excluding is 04:00 -> 09:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("04:00", "09:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "09:00", "10:00")
        )
      }

      "including is 05:00 -> 10:00 and excluding is 05:00 -> 09:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val excludingInterval = aTimeInterval("05:00", "09:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(excludingInterval))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "09:00", "10:00")
        )
      }
    }

    "Will return multiple intervals : (including.start -> excluding.start), (excluding.end -> including.end)" should {

      "including is 05:00 -> 10:00 and excluding is 06:00 -> 08:00" in {

        val includingInterval = aTimeInterval("05:00", "10:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(date -> List(aTimeInterval("06:00", "08:00")))

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )
        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "05:00", "06:00"),
          aTimeIntervalForDate("2019-04-08", "08:00", "10:00")
        )
      }

      "Will return 3 intervals " in {
        val includingInterval = aTimeInterval("05:00", "20:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(
            date -> List(
              aTimeInterval("06:00", "08:00"),
              aTimeInterval("09:30", "16:00"),
              aTimeInterval("19:00", "22:00"),
            )
          )

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )

        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "05:00", "06:00"),
          aTimeIntervalForDate("2019-04-08", "08:00", "09:30"),
          aTimeIntervalForDate("2019-04-08", "16:00", "19:00"),
        )
      }

      "Will return 4 intervals " in {
        val includingInterval = aTimeInterval("05:00", "20:00")
        val date              = aDate("2019-04-08")

        val exceptions: Map[LocalDate, List[TimeInterval]] =
          Map(
            date -> List(
              aTimeInterval("06:00", "08:00"),
              aTimeInterval("09:30", "16:00"),
              aTimeInterval("18:00", "19:00")
            )
          )

        val res =
          Intervals.applyExceptionsToInterval(exceptions, date)(
            includingInterval
          )

        res shouldBe List(
          aTimeIntervalForDate("2019-04-08", "05:00", "06:00"),
          aTimeIntervalForDate("2019-04-08", "08:00", "09:30"),
          aTimeIntervalForDate("2019-04-08", "16:00", "18:00"),
          aTimeIntervalForDate("2019-04-08", "19:00", "20:00")
        )
      }

    }
  }
}
