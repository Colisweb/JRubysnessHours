package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time._
import java.time.format.DateTimeFormatter

object SpecUtils {

  implicit class StringToLocalTimeOps(str: String) {
    def toLocalTime: LocalTime = LocalTime.parse(str)

    def -(to: String): TimeInterval =
      TimeInterval(str.toLocalTime, to.toLocalTime)

    def at(interval: TimeInterval): TimeIntervalForDate =
      TimeIntervalForDate(LocalDate.parse(str), interval)

    def at(time: String): ZonedDateTime =
      LocalDateTime.parse(s"$str $time", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")).atZone(zoneId)
  }

  implicit class LocalDateOps(localDate: LocalDate) {
    def ts(startTime: LocalTime, endTime: LocalTime): TimeIntervalForDate = {
      val t = TimeIntervalForDate(localDate, TimeInterval(startTime, endTime))
      println(t) //TODO: remove when enough tests are "tested"
      t
    }
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

  val schedule = Schedule(
    planning = planning,
    exceptions = Map.empty[LocalDate, List[TimeInterval]],
    timeZone = zoneId
  )

}
