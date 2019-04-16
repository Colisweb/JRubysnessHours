package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.ZoneOffset.UTC
import java.time._
import java.time.temporal.ChronoUnit

import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  implicit final class GenOps[A](private val self: Gen[A]) {
    def zip[B](other: Gen[B]): Gen[(A, B)] =
      for {
        a <- self
        b <- other
      } yield (a, b)
  }

  implicit final class GenTuple2Ops[A, B](private val self: Gen[(A, B)]) {
    def mapT[C](f: (A, B) => C): Gen[C]          = self.map(f.tupled)
    def flatMapT[C](f: (A, B) => Gen[C]): Gen[C] = self.flatMap(f.tupled)
  }

  implicit final class Tuple2Ops[A, B](private val self: (Gen[A], Gen[B])) {
    def map2[C](f: (A, B) => C): Gen[C] =
      for {
        a <- self._1
        b <- self._2
      } yield f(a, b)

    def flatMap2[C](f: (A, B) => Gen[C]): Gen[C] =
      self._1.zip(self._2).flatMap { case (a, b) => f(a, b) }
  }

  val LOCAL_DATE_MIN: LocalDate          = LocalDate.of(0, 1, 1)
  val LOCAL_DATE_MAX: LocalDate          = LocalDate.of(5000, 12, 31)
  val LOCAL_DATE_TIME_MIN: LocalDateTime = LocalDateTime.of(LOCAL_DATE_MIN, LocalTime.MIN)
  val LOCAL_DATE_TIME_MAX: LocalDateTime = LocalDateTime.of(LOCAL_DATE_MAX, LocalTime.MAX)

  import org.scalacheck.ops._

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

  val genDateTimeInterval: Gen[DateTimeInterval] =
    for {
      startSeconds <- chooseNum(LOCAL_DATE_TIME_MIN.toEpochSecond(UTC), LOCAL_DATE_TIME_MAX.toEpochSecond(UTC) - 1)
      startNanos   <- chooseNum(0, 999999999 - 1)

      start = LocalDateTime.ofEpochSecond(startSeconds, startNanos, UTC)

      endSeconds <- chooseNum(startSeconds + 1, start.plus(2, ChronoUnit.MONTHS).toEpochSecond(UTC))
      endNanos   <- chooseNum(startNanos + 1, 999999999)

      end = LocalDateTime.ofEpochSecond(endSeconds, endNanos, UTC)

    } yield DateTimeInterval(start = start, end = end)

  def genExceptionSurronding(date: ZonedDateTime, zoneId: ZoneId): Gen[Map[LocalDate, List[TimeInterval]]] = {
    val correctedDate = date.withZoneSameInstant(zoneId)
    for {
      interval <- genTimeIntervalSurrounding(correctedDate.toLocalTime)
    } yield Map(correctedDate.toLocalDate -> List(interval))
  }

  def genPlanningEntrySurrounding(date: ZonedDateTime, zoneId: ZoneId): Gen[Map[DayOfWeek, List[TimeInterval]]] = {
    val correctedDate = date.withZoneSameInstant(zoneId)
    for {
      interval <- genTimeIntervalSurrounding(correctedDate.toLocalTime)
    } yield Map(correctedDate.getDayOfWeek -> List(interval))
  }

  val genScheduler: Gen[Schedule] =
    for {
      planning   <- Gen.listOf(genTimeIntervalForWeekDay)
      exceptions <- Gen.listOf(genDateTimeInterval)
      zoneId     <- Arbitrary.arbitrary[ZoneId]
    } yield Schedule(planning = planning, exceptions = exceptions, timeZone = zoneId)

}
