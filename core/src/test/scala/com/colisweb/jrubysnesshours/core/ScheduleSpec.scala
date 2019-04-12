package com.colisweb.jrubysnesshours.core
import java.time.DayOfWeek._
import java.time.ZoneOffset.UTC
import java.time.{DayOfWeek, LocalDate, LocalDateTime, LocalTime}

import org.scalatest.{Matchers, WordSpec}

class ScheduleSpec extends WordSpec with Matchers {

  "Schedule" should {

    "index non-overlapping exceptions by day" in {
      val rawExceptions = List(
        aDateTimeInterval("2019-03-15", "10:00", "2019-03-15", "12:00"),
        aDateTimeInterval("2019-03-15", "13:00", "2019-03-15", "16:00"),
        aDateTimeInterval("2019-03-16", "10:00", "2019-03-16", "18:00"),
        aDateTimeInterval("2019-03-17", "15:00", "2019-03-17", "16:00"),
        aDateTimeInterval("2019-03-17", "18:00", "2019-03-17", "19:00"),
        aDateTimeInterval("2019-03-17", "08:00", "2019-03-17", "10:00"),
        aDateTimeInterval("2019-03-19", "17:00", "2019-03-19", "21:00"),
        aDateTimeInterval("2019-03-19", "10:00", "2019-03-19", "12:00")
      ).sortBy(_.hashCode())

      Schedule.apply(Nil, rawExceptions, UTC).exceptions should contain theSameElementsAs Map(
        aDate("2019-03-15") -> List(
          aTimeInterval("10:00", "12:00"),
          aTimeInterval("13:00", "16:00")
        ),
        aDate("2019-03-16") -> List(
          aTimeInterval("10:00", "18:00")
        ),
        aDate("2019-03-17") -> List(
          aTimeInterval("08:00", "10:00"),
          aTimeInterval("15:00", "16:00"),
          aTimeInterval("18:00", "19:00")
        ),
        aDate("2019-03-19") -> List(
          aTimeInterval("10:00", "12:00"),
          aTimeInterval("17:00", "21:00")
        )
      )
    }

    "index overlapping exceptions by day" in {
      val rawExceptions = List(
        aDateTimeInterval("2019-03-15", "10:00", "2019-03-15", "16:00"),
        aDateTimeInterval("2019-03-15", "13:00", "2019-03-15", "19:00"),
        aDateTimeInterval("2019-03-16", "10:00", "2019-03-17", "02:00"),
        aDateTimeInterval("2019-03-17", "01:00", "2019-03-17", "03:00"),
        aDateTimeInterval("2019-03-17", "11:00", "2019-03-17", "15:00"),
        aDateTimeInterval("2019-03-17", "12:00", "2019-03-17", "15:00"),
        aDateTimeInterval("2019-03-19", "08:00", "2019-03-19", "17:00"),
        aDateTimeInterval("2019-03-19", "08:00", "2019-03-19", "12:00")
      ).sortBy(_.hashCode())

      Schedule.apply(Nil, rawExceptions, UTC).exceptions should contain theSameElementsAs Map(
        aDate("2019-03-15") -> List(
          aTimeInterval("10:00", "19:00")
        ),
        aDate("2019-03-16") -> List(
          aTimeInterval("10:00", "23:59")
        ),
        aDate("2019-03-17") -> List(
          aTimeInterval("00:00", "03:00"),
          aTimeInterval("11:00", "15:00")
        ),
        aDate("2019-03-19") -> List(
          aTimeInterval("08:00", "17:00")
        )
      )
    }

    "index non-overlapping intervals by day-of-week" in {
      val rawPlanning = List(
        aTimeIntervalForWeekDay(MONDAY, "10:00", "12:00"),
        aTimeIntervalForWeekDay(MONDAY, "14:00", "18:00"),
        aTimeIntervalForWeekDay(WEDNESDAY, "08:00", "19:00"),
        aTimeIntervalForWeekDay(THURSDAY, "09:00", "10:00"),
        aTimeIntervalForWeekDay(THURSDAY, "13:00", "15:00"),
        aTimeIntervalForWeekDay(THURSDAY, "17:15", "18:15"),
        aTimeIntervalForWeekDay(SATURDAY, "09:15", "17:45")
      ).sortBy(_.hashCode())

      Schedule.apply(rawPlanning, Nil, UTC).planning should contain theSameElementsAs Map(
        MONDAY -> List(
          aTimeInterval("10:00", "12:00"),
          aTimeInterval("14:00", "18:00")
        ),
        WEDNESDAY -> List(
          aTimeInterval("08:00", "19:00")
        ),
        THURSDAY -> List(
          aTimeInterval("09:00", "10:00"),
          aTimeInterval("13:00", "15:00"),
          aTimeInterval("17:15", "18:15")
        ),
        SATURDAY -> List(
          aTimeInterval("09:15", "17:45")
        )
      )
    }

    "index overlapping intervals by day-of-week" in {
      val rawPlanning = List(
        aTimeIntervalForWeekDay(MONDAY, "10:00", "16:00"),
        aTimeIntervalForWeekDay(MONDAY, "14:00", "18:00"),
        aTimeIntervalForWeekDay(WEDNESDAY, "08:00", "19:00"),
        aTimeIntervalForWeekDay(WEDNESDAY, "10:00", "19:00"),
        aTimeIntervalForWeekDay(THURSDAY, "09:00", "13:00"),
        aTimeIntervalForWeekDay(THURSDAY, "12:00", "15:00"),
        aTimeIntervalForWeekDay(THURSDAY, "12:15", "18:15"),
        aTimeIntervalForWeekDay(THURSDAY, "20:00", "21:15")
      ).sortBy(_.hashCode())

      Schedule.apply(rawPlanning, Nil, UTC).planning should contain theSameElementsAs Map(
        MONDAY -> List(
          aTimeInterval("10:00", "18:00")
        ),
        WEDNESDAY -> List(
          aTimeInterval("08:00", "19:00")
        ),
        THURSDAY -> List(
          aTimeInterval("09:00", "18:15"),
          aTimeInterval("20:00", "21:15")
        )
      )
    }
  }

  def aDate(date: String): LocalDate = LocalDate.parse(date)

  def aTimeInterval(start: String, end: String): TimeInterval =
    TimeInterval(LocalTime.parse(start), LocalTime.parse(end))

  def aDateTimeInterval(startDate: String, startTime: String, endDate: String, endTime: String): DateTimeInterval =
    DateTimeInterval(LocalDateTime.parse(s"${startDate}T$startTime"), LocalDateTime.parse(s"${endDate}T$endTime"))

  def aTimeIntervalForWeekDay(dayOfWeek: DayOfWeek, startTime: String, endTime: String): TimeIntervalForWeekDay =
    TimeIntervalForWeekDay(dayOfWeek, TimeInterval(LocalTime.parse(startTime), LocalTime.parse(endTime)))
}
