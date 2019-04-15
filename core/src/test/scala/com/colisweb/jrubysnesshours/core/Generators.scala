package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.ZoneOffset.UTC
import java.time.{DayOfWeek, LocalDate, LocalDateTime, LocalTime, ZoneId, ZonedDateTime}

import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

object Generators {

  import org.scalacheck.ops._

  /**
    * Comes from here:
    *   https://github.com/scalatest/scalatest/issues/584#issuecomment-103952805
    */
  def noShrink[T](gen: Gen[T]): Gen[NoShrinkWrapper[T]] = gen.map(NoShrinkWrapper.apply)

  final case class NoShrinkWrapper[T](value: T) {
    override def toString: String = value.toString
  }
  object NoShrinkWrapper {
    implicit final def toT[T](noShrinkWrapper: NoShrinkWrapper[T]): T = noShrinkWrapper.value
  }

  type Planning   = Map[DayOfWeek, List[TimeInterval]]
  type Exceptions = Map[LocalDate, List[TimeInterval]]

  /**
    * I'm forced to do the `-1` and `+1` tricks because using ordinary LocalTime Gen creates too many invalid values.
    */
  def genTimeIntervalSurounding(localTime: LocalTime): Gen[TimeInterval] = {
    val l = localTime

    for {
      start <- chooseNum(LocalTime.MIN.toNanoOfDay, l.toNanoOfDay - 1)
      end   <- chooseNum(l.toNanoOfDay + 1, LocalTime.MAX.toNanoOfDay)
      startTime = LocalTime.ofNanoOfDay(start)
      endTime   = LocalTime.ofNanoOfDay(end)
    } yield {
      val res = TimeInterval(start = startTime, end = endTime)
      assert(res.contains(l), s"TOTO: ${res} - $l")
      res
    }
  }

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
      startSeconds <- chooseNum(LocalDateTime.MIN.toEpochSecond(UTC), LocalDateTime.MAX.toEpochSecond(UTC) - 1)
      startNanos   <- chooseNum(LocalDateTime.MIN.getNano, LocalDateTime.MAX.getNano - 1)
      start = LocalDateTime.ofEpochSecond(startSeconds, startNanos, UTC)

      startSeconds <- chooseNum(startSeconds + 1, LocalDateTime.MAX.toEpochSecond(UTC))
      startNanos   <- chooseNum(startNanos + 1, LocalDateTime.MAX.getNano)
      end = LocalDateTime.ofEpochSecond(startSeconds, startNanos, UTC)
    } yield DateTimeInterval(start = start, end = end)

  val genScheduleConstructor: Gen[(List[TimeIntervalForWeekDay], List[DateTimeInterval]) => Schedule] =
    for { zoneId <- Arbitrary.arbitrary[ZoneId] } yield
      (planning: List[TimeIntervalForWeekDay], exceptions: List[DateTimeInterval]) =>
        Schedule(planning = planning, exceptions = exceptions, timeZone = zoneId)

  val genScheduleConstructorContainingGeneratedZonedDateTime
    : Gen[List[DateTimeInterval] => (Schedule, ZonedDateTime)] = {
    def genTimeIntervalForWeekDaySurrounding(datetime: ZonedDateTime, zoneId: ZoneId): Gen[TimeIntervalForWeekDay] =
      for {
        interval <- genTimeIntervalSurounding(datetime.withZoneSameLocal(zoneId).toLocalTime)
      } yield TimeIntervalForWeekDay(dayOfWeek = datetime.withZoneSameLocal(zoneId).getDayOfWeek, interval = interval)

    for {
      zoneId             <- Arbitrary.arbitrary[ZoneId]
      datetime           <- Arbitrary.arbitrary[ZonedDateTime]
      planning           <- Gen.listOf(genTimeIntervalForWeekDay)
      intervalSurounding <- genTimeIntervalForWeekDaySurrounding(datetime, zoneId)
    } yield
      (exceptions: List[DateTimeInterval]) =>
        Schedule(planning = Random.shuffle(planning :+ intervalSurounding), exceptions = exceptions, timeZone = zoneId) -> datetime
  }

}
