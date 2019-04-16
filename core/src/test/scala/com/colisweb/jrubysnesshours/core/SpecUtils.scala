package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time._

object SpecUtils {

  implicit class DayOfWeekOps(dayOfWeek: DayOfWeek) {
    def at(interval: TimeInterval): TimeIntervalForWeekDay =
      TimeIntervalForWeekDay(dayOfWeek = dayOfWeek, interval = interval)
  }

  implicit class LocalDateOps(localDate: LocalDate) {
    def :-(hour: String): LocalDateTime = LocalDateTime.of(localDate, LocalTime.parse(hour))
  }

  implicit class LocalDateTimeOps(self: LocalDateTime) {
    def to(end: LocalDateTime): DateTimeInterval = DateTimeInterval(start = self, end = end)
  }

  implicit class StringToLocalTimeOps(string: String) {
    def toLocalTime: LocalTime = LocalTime.parse(string)
    def toLocalDate: LocalDate = LocalDate.parse(string)

    def -(to: String): TimeInterval = TimeInterval(string.toLocalTime, to.toLocalTime)

    def at(intervals: List[TimeInterval]): (LocalDate, List[TimeInterval]) =
      LocalDate.parse(string) -> intervals

    def :-(hour: String): LocalDateTime =
      LocalDateTime.of(LocalDate.parse(string), LocalTime.parse(hour))

    def at(interval: TimeInterval): TimeIntervalForDate =
      TimeIntervalForDate(date = LocalDate.parse(string), interval = interval)

    def at(time: String): ZonedDateTime = (string :- time).atZone(SpecUtils.zoneId)
  }

  val planning: Map[DayOfWeek, List[TimeInterval]] = Map(
    MONDAY    -> List("09:00" - "19:00"),
    TUESDAY   -> List("09:30" - "14:00", "15:00" - "19:00"),
    WEDNESDAY -> List("09:30" - "20:00"),
    THURSDAY  -> List("09:30" - "19:00"),
    FRIDAY    -> List("09:30" - "19:00"),
    SATURDAY  -> List("09:00" - "14:00", "15:00" - "19:00")
  )

  val zoneId: ZoneId = ZoneId.of("Europe/Paris")

  val schedule: Schedule =
    Schedule(
      planning = planning,
      exceptions = Map.empty[LocalDate, List[TimeInterval]],
      timeZone = zoneId
    )

  val TimeIntervalMin: String = LocalTime.MIN.toString
  val TimeIntervalMax: String = LocalTime.MAX.toString
}
