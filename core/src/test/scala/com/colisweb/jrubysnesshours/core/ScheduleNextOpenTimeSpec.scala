package com.colisweb.jrubysnesshours.core
import java.time.LocalDate.parse
import java.time.ZoneId

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScheduleNextOpenTimeSpec extends AnyWordSpec with Matchers {
  import SpecUtils._

  "Schedule#nextOpenTimeAfter" should {
    "return no nextOpenTime when the planning is empty" in {
      val emptySchedule = Schedule(Nil, Nil, ZoneId.of(FRANCE_TIMEZONE))
      emptySchedule.nextOpenTimeAfter("2019-03-15" at "15:00") shouldBe None
      emptySchedule.nextOpenTimeAfter("2019-03-16" at "14:30") shouldBe None
    }

    "return a nextOpenTime in the same day" in {
      schedule.nextOpenTimeAfter("2019-03-15" at "15:00") shouldBe Some("2019-03-15" at "15:00")
      schedule.nextOpenTimeAfter("2019-03-15" at "20:00") shouldBe Some("2019-03-16" at "09:00")
      schedule.nextOpenTimeAfter("2019-03-16" at "14:00") shouldBe Some("2019-03-16" at "15:00")
      schedule.nextOpenTimeAfter("2019-03-16" at "14:30") shouldBe Some("2019-03-16" at "15:00")
    }

    "return a nextOpenTime in the next days" in {
      schedule.nextOpenTimeAfter("2019-03-16" at "20:00") shouldBe Some("2019-03-18" at "09:00")
      schedule.nextOpenTimeAfter("2019-03-16" at "19:00") shouldBe Some("2019-03-18" at "09:00")
      schedule.nextOpenTimeAfter("2019-03-17" at "12:00") shouldBe Some("2019-03-18" at "09:00")
    }

    "return a nextOpenTime in the same day with exceptions" in {
      val scheduleWithExceptions = schedule.copy(
        exceptions = Map(
          parse("2019-03-11") -> List("13:00" - "16:00"),
          parse("2019-03-12") -> List("09:00" - "14:00")
        )
      )
      scheduleWithExceptions.nextOpenTimeAfter("2019-03-11" at "13:00") shouldBe Some("2019-03-11" at "16:00")
      scheduleWithExceptions.nextOpenTimeAfter("2019-03-12" at "09:30") shouldBe Some("2019-03-12" at "15:00")
    }

    "return a nextOpenTime in the next days with exceptions" in {
      val scheduleWithExceptions = schedule.copy(
        exceptions = Map(
          parse("2019-03-12") -> List("09:00" - "14:00"),
          parse("2019-03-13") -> List("08:00" - "20:00"),
          parse("2019-03-14") -> List("08:00" - "20:00")
        )
      )
      scheduleWithExceptions.nextOpenTimeAfter("2019-03-11" at "19:00") shouldBe Some("2019-03-12" at "15:00")
      scheduleWithExceptions.nextOpenTimeAfter("2019-03-12" at "19:30") shouldBe Some("2019-03-15" at "09:30")
      scheduleWithExceptions.nextOpenTimeAfter("2019-03-14" at "09:45") shouldBe Some("2019-03-15" at "09:30")
    }

    "return a nextOpenTime after a long period closed by exceptions" in {
      val startDate = parse("2019-03-12")
      val scheduleWithExceptions =
        schedule.copy(exceptions = (0 until 60).map(i => startDate.plusDays(i.toLong) -> List("08:00" - "21:00")).toMap)
      scheduleWithExceptions.nextOpenTimeAfter("2019-03-11" at "19:30") shouldBe Some("2019-05-11" at "09:00")
    }
  }
}
