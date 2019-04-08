package com.colisweb.jrubysnesshours.core

import java.time.{DayOfWeek, LocalDate, LocalDateTime, LocalTime}

import com.colisweb.jrubysnesshours.core.Core.{BusinessHour, Interval, TimeSegment}
import org.scalatest.{Matchers, WordSpec}

class TimeSegmentTest  extends WordSpec with Matchers {

  "Some business hours with interval exception in 2019/04/08" when {

    val businessHours =
      List(
        BusinessHour(DayOfWeek.MONDAY, Interval(LocalTime.of(9, 0), LocalTime.of(19, 0))),

        BusinessHour(DayOfWeek.TUESDAY, Interval(LocalTime.of(9, 30), LocalTime.of(14, 0))),
        BusinessHour(DayOfWeek.TUESDAY, Interval(LocalTime.of(15, 0), LocalTime.of(19, 0))),

        BusinessHour(DayOfWeek.WEDNESDAY, Interval(LocalTime.of(9, 30), LocalTime.of(20, 0))),

        BusinessHour(DayOfWeek.THURSDAY, Interval(LocalTime.of(9, 30), LocalTime.of(19, 0))),

        BusinessHour(DayOfWeek.FRIDAY, Interval(LocalTime.of(9, 30), LocalTime.of(19, 0))),

        BusinessHour(DayOfWeek.SATURDAY, Interval(LocalTime.of(9, 0), LocalTime.of(14, 0))),
        BusinessHour(DayOfWeek.SATURDAY, Interval(LocalTime.of(15, 0), LocalTime.of(19, 0)))
      )

    val businessHoursByDayOfWeek = BusinessHour.toBusinessHoursForDayOfWeek(businessHours)
    val intervalException = List(
      TimeSegment(
        LocalDate.of(2019, 4, 8),
        LocalDateTime.of(2019, 4, 8, 13, 0),
        LocalDateTime.of(2019, 4, 8, 16, 0)
      )
    )
    // TODO do one test with and without exceptions

    "TimeSegment.segmentBetween 2019/04/05 13h40 and 2019/04/09 13h40" in {
      val start = LocalDateTime.of(2019, 4, 5, 13, 40)
      val end = LocalDateTime.of(2019, 4, 9, 13, 40)

      val res = TimeSegment.segmentBetween(start, end, businessHoursByDayOfWeek, intervalException)

      val expected =
        Vector(
          TimeSegment(LocalDate.of(2019, 4, 5), LocalDateTime.of(2019, 4, 5, 13, 40), LocalDateTime.of(2019, 4, 5, 19, 0)),
          TimeSegment(LocalDate.of(2019, 4, 6), LocalDateTime.of(2019, 4, 6, 9, 0), LocalDateTime.of(2019, 4, 6, 14, 0)),
          TimeSegment(LocalDate.of(2019, 4, 6), LocalDateTime.of(2019, 4, 6, 15, 0), LocalDateTime.of(2019, 4, 6, 19, 0)),
          TimeSegment(LocalDate.of(2019, 4, 8), LocalDateTime.of(2019, 4, 8, 9, 0), LocalDateTime.of(2019, 4, 8, 13, 0)),
          TimeSegment(LocalDate.of(2019, 4, 8), LocalDateTime.of(2019, 4, 8, 16, 0), LocalDateTime.of(2019, 4, 8, 19, 0)),
          TimeSegment(LocalDate.of(2019, 4, 9),LocalDateTime.of(2019, 4, 9, 9, 30), LocalDateTime.of(2019, 4, 9, 13, 40))
        )

      res shouldEqual expected
    }
  }

  LocalDate.of(2019, 4, 8)


  "Merging mergeTimeSegments" when {


    "Some overlapping TimeSegments ( for the same day )" in {

      val timeSegments =
        Seq(
          TimeSegment(
            LocalDate.of(2019, 4, 8),
            LocalDateTime.of(2019, 4, 8, 2, 0),
            LocalDateTime.of(2019, 4, 8, 5, 0)
          ),
          TimeSegment(
            LocalDate.of(2019, 4, 8),
            LocalDateTime.of(2019, 4, 8, 4, 0),
            LocalDateTime.of(2019, 4, 8, 8, 0)
          ),
          TimeSegment(
            LocalDate.of(2019, 4, 8),
            LocalDateTime.of(2019, 4, 8, 9, 0),
            LocalDateTime.of(2019, 4, 8, 19, 0)
          ),
          TimeSegment(
            LocalDate.of(2019, 4, 8),
            LocalDateTime.of(2019, 4, 8, 13, 0),
            LocalDateTime.of(2019, 4, 8, 16, 0)
          ),
        )

      val expected =
        List(
          TimeSegment(LocalDate.of(2019, 4, 8), LocalDateTime.of(2019, 4, 8, 2, 0), LocalDateTime.of(2019, 4, 8, 8, 0)),
          TimeSegment(LocalDate.of(2019, 4, 8), LocalDateTime.of(2019, 4, 8, 9, 0), LocalDateTime.of(2019, 4, 8, 19, 0))
        )

      TimeSegment.mergeTimeSegments(timeSegments) shouldEqual expected





    }
  }

}
