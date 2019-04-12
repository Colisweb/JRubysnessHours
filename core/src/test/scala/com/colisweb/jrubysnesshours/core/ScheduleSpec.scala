package com.colisweb.jrubysnesshours.core
import java.time.ZoneOffset.UTC
import java.time.{LocalDate, LocalDateTime, LocalTime}

import com.colisweb.jrubysnesshours.core.Core.{DateTimeInterval, Schedule, TimeInterval}
import org.scalatest.{Matchers, WordSpec}

class ScheduleSpec extends WordSpec with Matchers {

  "dateTimeIntervalsToExceptions" should {

    "index non-overlapping intervals by day" in {
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

    "index overlapping intervals by day" in {
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
  }

  def aDate(date: String): LocalDate = LocalDate.parse(date)

  def aTimeInterval(start: String, end: String): TimeInterval =
    TimeInterval.of(LocalTime.parse(start), LocalTime.parse(end))

  def aDateTimeInterval(startDate: String, startTime: String, endDate: String, endTime: String): DateTimeInterval =
    DateTimeInterval(LocalDateTime.parse(s"${startDate}T$startTime"), LocalDateTime.parse(s"${endDate}T$endTime"))
}
