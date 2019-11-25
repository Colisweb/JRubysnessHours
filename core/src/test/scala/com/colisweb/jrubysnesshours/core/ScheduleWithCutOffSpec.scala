package com.colisweb.jrubysnesshours.core

import com.colisweb.jrubysnesshours.core.SpecUtils._
import com.github.writethemfirst.Approbation
import org.scalatest.{Matchers, fixture}

class ScheduleWithCutOffSpec extends fixture.FlatSpec with Matchers with Approbation {
  val cutOff = Some(
    DoubleCutOff(
      sameDay = CutOff(limit = "08:00".toLocalTime, firstAvailableTime = "17:00".toLocalTime),
      nextDay = CutOff(limit = "12:00".toLocalTime, firstAvailableTime = "15:00".toLocalTime)
    )
  )

  "a Schedule" should "cut start of D day" in { approver =>
    val slots = schedule.splitTimeSegments(
      "2019-05-02" at "10:00" -> FRANCE_TIMEZONE,
      "2019-05-12" at "10:00" -> FRANCE_TIMEZONE,
      2.hours,
      cutOff
    )
    approver.verify(prettify(slots))
  }

  it should "keep start of D day" in { approver =>
    val slots = schedule.splitTimeSegments(
      "2019-05-02" at "08:00" -> FRANCE_TIMEZONE,
      "2019-05-12" at "10:00" -> FRANCE_TIMEZONE,
      2.hours,
      cutOff
    )
    approver.verify(prettify(slots))
  }

  it should "cut start of D plus 1 day" in { approver =>
    val slots = schedule.splitTimeSegments(
      "2019-05-02" at "12:01" -> FRANCE_TIMEZONE,
      "2019-05-12" at "10:00" -> FRANCE_TIMEZONE,
      2.hours,
      cutOff
    )
    approver.verify(prettify(slots))
  }

  it should "cut start of next open day and skip exceptions" in { approver =>
    val slots = schedule
      .copy(exceptions = Map("2019-05-08".toLocalDate -> List(TimeInterval.FULL_DAY)))
      .splitTimeSegments(
        "2019-05-07" at "12:01" -> FRANCE_TIMEZONE,
        "2019-05-11" at "10:00" -> FRANCE_TIMEZONE,
        2.hours,
        cutOff
      )
    approver.verify(prettify(slots))
  }
}
