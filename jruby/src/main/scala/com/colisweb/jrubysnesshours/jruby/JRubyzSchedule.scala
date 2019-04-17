package com.colisweb.jrubysnesshours.jruby

import java.time.DayOfWeek._
import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalTime, ZoneId, ZonedDateTime}

import com.colisweb.jrubysnesshours.core._

final case class RubyTimeSegmentInterval(date: String, startTime: String, endTime: String)

final class JRubyzSchedule private[jruby] (schedule: Schedule) {

  import JRubyzSchedule._

  def timeSegments(startsAt: String, endsAt: String): Array[RubyTimeSegmentInterval] =
    schedule
      .intervalsBetween(ZonedDateTime.parse(startsAt), ZonedDateTime.parse(endsAt))
      .map { timeIntervalForDate =>
        val start: ZonedDateTime =
          ZonedDateTime
            .of(timeIntervalForDate.date, timeIntervalForDate.start, schedule.timeZone)
            .withZoneSameInstant(UTC)

        val end =
          ZonedDateTime
            .of(timeIntervalForDate.date, timeIntervalForDate.end, schedule.timeZone)
            .withZoneSameInstant(UTC)

        RubyTimeSegmentInterval(
          date = timeIntervalForDate.date.format(ISO_DATE_FORMATTER),
          startTime = start.format(ISO_8601_FORMATTER),
          endTime = end.format(ISO_8601_FORMATTER)
        )
      }
      .toArray

  def contains(start: String, end: String): Boolean = {
    val startZonedDateTime = ZonedDateTime.parse(start)
    val endZonedDateTime   = ZonedDateTime.parse(end)

    val startLocalDate = startZonedDateTime.toLocalDate
    val endLocalDate   = endZonedDateTime.toLocalDate

    // TODO : il faudra gérer le cas ou on passe un start et un end qui ne sont pas sur le meme jour.
    //  Solution possible, merger le résultat de intervalBetween sans se soucier de la date

    if (startLocalDate == endLocalDate) {
      val timeIntervalForDate =
        TimeIntervalForDate(
          date = startLocalDate,
          interval = TimeInterval(start = startZonedDateTime.toLocalTime, end = endZonedDateTime.toLocalTime)
        )

      schedule.contains(timeIntervalForDate)
    } else false
  }

  def isOpen(time: String): Boolean = schedule.contains(ZonedDateTime.parse(time))

  def nextOpentime(time: String): String =
    schedule
      .nextOpenTimeAfter(ZonedDateTime.parse(time))
      .map(_.withZoneSameInstant(UTC).format(ISO_8601_FORMATTER))
      .orNull

}

object JRubyzSchedule {

  private val UTC                = ZoneId.of("UTC")
  private val ISO_DATE_FORMATTER = DateTimeFormatter.ISO_DATE
  private val ISO_8601_FORMATTER = DateTimeFormatter.ISO_DATE_TIME

  def exception(startsAt: String, endsAt: String): DateTimeInterval =
    DateTimeInterval(
      start = ZonedDateTime.parse(startsAt).toLocalDateTime,
      end = ZonedDateTime.parse(endsAt).toLocalDateTime
    )

  def planningEntry(rubyWeekDay: Int, startTime: String, endTime: String): TimeIntervalForWeekDay =
    TimeIntervalForWeekDay(
      dayOfWeek = rubyWeekDayToJavaWeekDay(rubyWeekDay),
      interval = TimeInterval(start = LocalTime.parse(startTime), end = LocalTime.parse(endTime))
    )

  def schedule(
      plannings: Array[TimeIntervalForWeekDay],
      exceptions: Array[DateTimeInterval],
      timeZone: String
  ): JRubyzSchedule =
    new JRubyzSchedule(
      Schedule(planning = plannings.toList, exceptions = exceptions.toList, timeZone = ZoneId.of(timeZone))
    )

  private[jruby] def rubyWeekDayToJavaWeekDay(rubyWeekDay: Int): DayOfWeek =
    if (rubyWeekDay == 0) SUNDAY else DayOfWeek.of(rubyWeekDay)

}
