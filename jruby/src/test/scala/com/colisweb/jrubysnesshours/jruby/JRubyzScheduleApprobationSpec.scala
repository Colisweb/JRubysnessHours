package com.colisweb.jrubysnesshours.jruby

import java.time.{LocalTime, ZonedDateTime}

import com.colisweb.Approbation
import com.colisweb.jrubysnesshours.core.{CutOff, DoubleCutOff}
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule._
import com.colisweb.jrubysnesshours.jruby.SampleSchedule._
import com.colisweb.jrubysnesshours.jruby.SpecUtils._
import com.github.writethemfirst.approvals.utils.FunctionUtils

import scala.collection.JavaConverters._

class JRubyzScheduleApprobationSpec extends Approbation {

  "jrubySchedule" should "split on 2 weeks" in { approver =>
    val segments = jrubySchedule.splitTimeSegments("2019-05-06T11:50:39Z", "2019-05-20T16:17:39Z", 2.hours, None)

    approver.verify(prettify(segments))
  }

  it should "split in segments of 30 minutes" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-06T12:20:39Z", "2019-05-07T16:47:39Z", 30.minutes, None)

    approver.verify(prettify(segments))
  }

  it should "split in segments of 90 minutes" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-06T12:20:39Z", "2019-05-07T16:47:39Z", 90.minutes, None)

    approver.verify(prettify(segments))
  }

  it should "split in segments of 2h15 minutes" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-06T12:20:39Z", "2019-05-07T16:47:39Z", 135.minutes, None)

    approver.verify(prettify(segments))
  }

  val cutOff = Some(
    DoubleCutOff(
      sameDay = CutOff(limit = "08:00", firstAvailableTime = "16:00"),
      nextDay = CutOff(limit = "12:00", firstAvailableTime = "15:00")
    )
  )

  it should "not cut-off when before first time" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-06T06:50:39Z", "2019-05-09T16:17:39Z", 2.hours, cutOff)
    approver.verify(prettify(segments))
  }

  it should "cut-off when between 2 times" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-06T10:02:39Z", "2019-05-09T16:17:39Z", 2.hours, cutOff)
    approver.verify(prettify(segments))
  }

  it should "cut-off when after 2nd time" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-06T12:08:39Z", "2019-05-09T16:17:39Z", 2.hours, cutOff)
    approver.verify(prettify(segments))
  }

  it should "cut-off when after 2nd time and the day before a partial exception" in { approver =>
    val segments =
      jrubySchedule.splitTimeSegments("2019-05-07T12:08:39Z", "2019-05-10T16:17:39Z", 2.hours, cutOff)
    approver.verify(prettify(segments))
  }

  it should "cut-off when after 2nd time and the day before a full exception" in { approver =>
    val scheduleWithException =
      schedule(sixDays12_18, Array(exception("2019-05-08T00:00:00Z", "2019-05-08T23:00:00Z")), "UTC")
    val segments = scheduleWithException
      .splitTimeSegments("2019-05-07T12:08:39Z", "2019-05-10T16:17:39Z", 2.hours, cutOff)
    approver.verify(prettify(segments))
  }

  it should "convert days" in { approver =>
    approver.verify(FunctionUtils.applyCombinations((0 to 6).asJava, rubyWeekDayToJavaWeekDay))
  }

  implicit def toZDT(s: String): ZonedDateTime = ZonedDateTime.parse(s)
  implicit def toLocal(s: String): LocalTime   = LocalTime.parse(s)
}
