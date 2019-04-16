package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.ZoneOffset.UTC
import java.time._
import java.time.temporal.ChronoUnit

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.chooseNum

import scala.util.Random

object Generators {

  val LOCAL_DATE_MIN: LocalDate          = LocalDate.of(0, 1, 1)
  val LOCAL_DATE_MAX: LocalDate          = LocalDate.of(5000, 12, 31)
  val LOCAL_DATE_TIME_MIN: LocalDateTime = LocalDateTime.of(LOCAL_DATE_MIN, LocalTime.MIN)
  val LOCAL_DATE_TIME_MAX: LocalDateTime = LocalDateTime.of(LOCAL_DATE_MAX, LocalTime.MAX)

  import org.scalacheck.ops._

  def genBoundedLocalDate: Gen[LocalDate] =
    for {
      date <- chooseNum(LOCAL_DATE_MIN.toEpochDay, LOCAL_DATE_MAX.toEpochDay)
    } yield LocalDate.ofEpochDay(date)

  def genBoundedZonedDateTime: Gen[ZonedDateTime] =
    for {
      zoneId <- Arbitrary.arbitrary[ZoneId]
      date   <- genBoundedLocalDate
      time   <- Arbitrary.arbitrary[LocalTime]
    } yield ZonedDateTime.of(date, time, zoneId)

  def genTimeIntervalSurrounding(localTime: LocalTime): Gen[TimeInterval] =
    if (localTime == LocalTime.MIN) Gen.fail
    else if (localTime == LocalTime.MAX) Gen.fail
    else
      for {
        start <- chooseNum(LocalTime.MIN.toNanoOfDay, localTime.toNanoOfDay - 1)
        end   <- chooseNum(localTime.toNanoOfDay + 1, LocalTime.MAX.toNanoOfDay)
      } yield TimeInterval(start = LocalTime.ofNanoOfDay(start), end = LocalTime.ofNanoOfDay(end))

  val genTimeInterval: Gen[TimeInterval] =
    for {
      start <- chooseNum(LocalTime.MIN.toNanoOfDay, LocalTime.MAX.toNanoOfDay - 1)
      end   <- chooseNum(start + 1, LocalTime.MAX.toNanoOfDay)
    } yield TimeInterval(start = LocalTime.ofNanoOfDay(start), end = LocalTime.ofNanoOfDay(end))

  val genDayOfWeek: Gen[DayOfWeek] = Gen.oneOf(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)

  val genTimeIntervalForWeekDay: Gen[TimeIntervalForWeekDay] =
    for {
      dow      <- genDayOfWeek
      interval <- genTimeInterval
    } yield TimeIntervalForWeekDay(dayOfWeek = dow, interval = interval)

  def genDateTimeInterval: Gen[DateTimeInterval] =
    for {
      startSeconds <- chooseNum(LOCAL_DATE_TIME_MIN.toEpochSecond(UTC), LOCAL_DATE_TIME_MAX.toEpochSecond(UTC) - 1)
      startNanos   <- chooseNum(0, 999999999 - 1)

      start = LocalDateTime.ofEpochSecond(startSeconds, startNanos, UTC)

      endSeconds <- chooseNum(startSeconds + 1, start.plus(2, ChronoUnit.MONTHS).toEpochSecond(UTC))
      endNanos   <- chooseNum(startNanos + 1, 999999999)

      end = LocalDateTime.ofEpochSecond(endSeconds, endNanos, UTC)

    } yield DateTimeInterval(start = start, end = end)

  val genScheduleConstructor: Gen[(List[TimeIntervalForWeekDay], List[DateTimeInterval]) => Schedule] =
    for { zoneId <- Arbitrary.arbitrary[ZoneId] } yield
      (planning: List[TimeIntervalForWeekDay], exceptions: List[DateTimeInterval]) =>
        Schedule(planning = planning, exceptions = exceptions, timeZone = zoneId)

  val genScheduleConstructorContainingGeneratedZonedDateTime: Gen[(Schedule, ZonedDateTime)] = {
    def genTimeIntervalForWeekDaySurrounding(datetime: ZonedDateTime, zoneId: ZoneId): Gen[TimeIntervalForWeekDay] =
      for {
        interval <- genTimeIntervalSurrounding(datetime.withZoneSameInstant(zoneId).toLocalTime)
      } yield TimeIntervalForWeekDay(dayOfWeek = datetime.withZoneSameInstant(zoneId).getDayOfWeek, interval = interval)

    for {
      zoneId             <- Arbitrary.arbitrary[ZoneId]
      datetime           <- genBoundedZonedDateTime
      planning           <- Gen.listOf(genTimeIntervalForWeekDay)
      intervalSurounding <- genTimeIntervalForWeekDaySurrounding(datetime, zoneId)
    } yield
      (
        Schedule(planning = Random.shuffle(planning :+ intervalSurounding), exceptions = Nil, timeZone = zoneId),
        datetime
      )
  }

}
