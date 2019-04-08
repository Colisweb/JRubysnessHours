package com.colisweb.jrubysnesshours.core

import java.time._
import java.time.temporal.ChronoUnit.MINUTES

import com.colisweb.jrubysnesshours.core.Core.BusinessHoursByDayOfWeek

object Within {

  case class DayOfWeekTime(dayOfWeek: DayOfWeek, time: LocalTime)

  def within(
      planning: BusinessHoursByDayOfWeek,
      planningTimeZone: ZoneId
  )(start: ZonedDateTime, end: ZonedDateTime): Duration = {
    val localStart = start.withZoneSameInstant(planningTimeZone).toLocalDateTime
    val localEnd = end.withZoneSameInstant(planningTimeZone).toLocalDateTime

    if (localStart.toLocalDate == localEnd.toLocalDate) {
      withinOneDay(planning)(
        DayOfWeekTime(localStart.getDayOfWeek, localStart.toLocalTime),
        DayOfWeekTime(localEnd.getDayOfWeek, localEnd.toLocalTime)
      )
    } else {
      val startDuration = withinStartDay(planning)(
        DayOfWeekTime(localStart.getDayOfWeek, localStart.toLocalTime)
      )
      val endDuration = withinEndDay(planning)(
        DayOfWeekTime(localEnd.getDayOfWeek, localEnd.toLocalTime)
      )

      val numberOfDays =
        Period.between(localStart.toLocalDate, localEnd.toLocalDate).getDays
      val dayRangeDuration = Range(1, numberOfDays)
        .foldLeft(Duration.ZERO) { (total, i) =>
          val day = localStart.plusDays(i.toLong).getDayOfWeek
          total.plus(dayDuration(planning)(day))
        }

      startDuration.plus(dayRangeDuration).plus(endDuration)
    }
  }

  private def withinStartDay(
      planning: BusinessHoursByDayOfWeek
  )(start: DayOfWeekTime): Duration = {
    planning
      .getOrElse(start.dayOfWeek, Nil)
      .foldLeft(Duration.ZERO) { (total, interval) =>
        if (interval.endTime.isBefore(start.time))
          total
        else if (interval.startTime.isBefore(start.time))
          total.plusMinutes(start.time.until(interval.endTime, MINUTES))
        else
          total.plusMinutes(interval.startTime.until(interval.endTime, MINUTES))
      }
  }

  private def withinEndDay(
      planning: BusinessHoursByDayOfWeek
  )(end: DayOfWeekTime): Duration = {
    planning
      .getOrElse(end.dayOfWeek, Nil)
      .foldLeft(Duration.ZERO) { (total, interval) =>
        if (interval.startTime.isAfter(end.time))
          total
        else if (interval.endTime.isAfter(end.time))
          total.plusMinutes(interval.startTime.until(end.time, MINUTES))
        else
          total.plusMinutes(interval.startTime.until(interval.endTime, MINUTES))
      }
  }

  private def withinOneDay(
      planning: BusinessHoursByDayOfWeek
  )(start: DayOfWeekTime, end: DayOfWeekTime): Duration = {
    planning
      .getOrElse(start.dayOfWeek, Nil)
      .foldLeft(Duration.ZERO) { (total, interval) =>
        if (interval.endTime.isBefore(start.time) || interval.startTime.isAfter(
              end.time
            ))
          total
        else if (interval.startTime.isBefore(start.time) && interval.endTime
                   .isAfter(end.time))
          total.plusMinutes(start.time.until(end.time, MINUTES))
        else if (interval.startTime.isBefore(start.time))
          total.plusMinutes(start.time.until(interval.endTime, MINUTES))
        else if (interval.endTime.isAfter(end.time))
          total.plusMinutes(interval.startTime.until(end.time, MINUTES))
        else
          total.plusMinutes(interval.startTime.until(interval.endTime, MINUTES))
      }
  }

  private def dayDuration(
      planning: BusinessHoursByDayOfWeek
  )(dayOfWeek: DayOfWeek): Duration = {
    planning
      .getOrElse(dayOfWeek, Nil)
      .foldLeft(Duration.ZERO)(
        (total, interval) =>
          total.plusMinutes(interval.startTime.until(interval.endTime, MINUTES))
      )
  }
}
