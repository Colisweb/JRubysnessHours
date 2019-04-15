package com.colisweb.jrubysnesshours.core

import java.time.DayOfWeek._
import java.time.{DayOfWeek, LocalDate, LocalTime, ZoneId}

import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  import org.scalacheck.ops._

  /**
    * I'm forced to do the `-1` and `+1` tricks because using ordinary LocalTime Gen creates too many invalid values.
    */
  val genTimeInterval: Gen[TimeInterval] =
    for {
      start <- chooseNum(LocalTime.MIN.toNanoOfDay, LocalTime.MAX.toNanoOfDay - 1)
      end   <- chooseNum(start + 1, LocalTime.MAX.toNanoOfDay)
    } yield TimeInterval(start = LocalTime.ofNanoOfDay(start), end = LocalTime.ofNanoOfDay(end))

  val genDayOfWeek: Gen[DayOfWeek] = Gen.oneOf(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)

  val genPlanningDay: Gen[(DayOfWeek, List[TimeInterval])] =
    for {
      dow       <- genDayOfWeek
      intervals <- Gen.listOf(genTimeInterval)
    } yield (dow, intervals)

  def genPlannings(size: Int): Gen[Map[DayOfWeek, List[TimeInterval]]] =
    Gen.listOfN(size, genPlanningDay).map(_.toMap)

  val genNonEmptyPlannings: Gen[Map[DayOfWeek, List[TimeInterval]]] =
    Gen.nonEmptyListOf(genPlanningDay).map(_.toMap)

  val genExceptionDay: Gen[(LocalDate, List[TimeInterval])] =
    for {
      date      <- Arbitrary.arbitrary[LocalDate]
      intervals <- Gen.listOf(genTimeInterval)
    } yield (date, intervals)

  def genExceptions(size: Int): Gen[Map[LocalDate, List[TimeInterval]]] =
    Gen.listOfN(size, genExceptionDay).map(_.toMap)

  val genNonEmptyExceptions: Gen[Map[LocalDate, List[TimeInterval]]] =
    Gen.nonEmptyListOf(genExceptionDay).map(_.toMap)

  def genSchedule(planningSize: Int, exceptionsSize: Int): Gen[Schedule] =
    for {
      zoneId     <- Arbitrary.arbitrary[ZoneId]
      planning   <- genPlannings(planningSize)
      exceptions <- genExceptions(exceptionsSize)
    } yield new Schedule(planning = planning, exceptions = exceptions, timeZone = zoneId)

  val genScheduleConstructor
    : Gen[(Map[DayOfWeek, List[TimeInterval]], Map[LocalDate, List[TimeInterval]]) => Schedule] =
    for { zoneId <- Arbitrary.arbitrary[ZoneId] } yield
      (planning: Map[DayOfWeek, List[TimeInterval]], exceptions: Map[LocalDate, List[TimeInterval]]) =>
        new Schedule(planning = planning, exceptions = exceptions, timeZone = zoneId)

}
