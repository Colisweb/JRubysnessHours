package com.colisweb.jrubysnesshours.jruby

import java.time.ZonedDateTime

import com.colisweb.Approbation
import com.colisweb.jrubysnesshours.core.{CutOff, DoubleCutOff}
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule.rubyWeekDayToJavaWeekDay
import com.colisweb.jrubysnesshours.jruby.SampleSchedule.jrubySchedule
import com.colisweb.jrubysnesshours.jruby.SpecUtils._
import com.github.writethemfirst.approvals.utils.FunctionUtils

import scala.collection.JavaConverters._

class JRubyzScheduleApprobationSpec extends Approbation {

  "jrubySchedule" should "split on 2 weeks" in { approver =>
    val segments = jrubySchedule.splitTimeSegments(
      ZonedDateTime.parse("2019-05-06T11:50:39Z"),
      ZonedDateTime.parse("2019-05-20T16:17:39Z"),
      2
    )

    approver.verify(prettify(segments))
  }

  val cutOff = Some(
    DoubleCutOff(
      sameDay = CutOff(limit = "08:00".toLocalTime, firstAvailableTime = "16:00".toLocalTime),
      nextDay = CutOff(limit = "12:00".toLocalTime, firstAvailableTime = "15:00".toLocalTime)
    )
  )

  it should "not cut-off when before first time" in { approver =>
    val segments = jrubySchedule.splitTimeSegments(
      ZonedDateTime.parse("2019-05-06T06:50:39Z"),
      ZonedDateTime.parse("2019-05-09T16:17:39Z"),
      2,
      cutOff
    )
    approver.verify(prettify(segments))
  }

  it should "cut-off when between 2 times" in { approver =>
    val segments = jrubySchedule.splitTimeSegments(
      ZonedDateTime.parse("2019-05-06T10:02:39Z"),
      ZonedDateTime.parse("2019-05-09T16:17:39Z"),
      2,
      cutOff
    )
    approver.verify(prettify(segments))
  }

  it should "cut-off when after 2nd time" in { approver =>
    val segments = jrubySchedule.splitTimeSegments(
      ZonedDateTime.parse("2019-05-06T12:08:39Z"),
      ZonedDateTime.parse("2019-05-09T16:17:39Z"),
      2,
      cutOff
    )
    approver.verify(prettify(segments))
  }

  it should "convert days" in { approver =>
    approver.verify(FunctionUtils.applyCombinations((0 to 6).asJava, rubyWeekDayToJavaWeekDay))
  }
}
