package com.colisweb.jrubysnesshours.core

import java.time.{Duration, LocalDate, LocalTime, ZoneId, ZonedDateTime}

import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import com.fortysevendeg.scalacheck.datetime.instances.jdk8._
import org.scalacheck.Gen

object Generators {
  val from: ZonedDateTime = ZonedDateTime.of(2000, 1, 1, 0, 0, 0, 0, ZoneId.of("UTC"))
  val range: Duration     = Duration.ofDays(10000)

  val genDate: Gen[ZonedDateTime]         = genDateTimeWithinRange(from, range)
  val genLocalTime: Gen[LocalTime]        = genDateTimeWithinRange(from, range).map(_.toLocalTime)
  val genLocalDate: Gen[LocalDate]        = genDateTimeWithinRange(from, range).map(_.toLocalDate)
  val gen4LocalTimes: Gen[Seq[LocalTime]] = Gen.listOfN(4, genLocalTime)
}
