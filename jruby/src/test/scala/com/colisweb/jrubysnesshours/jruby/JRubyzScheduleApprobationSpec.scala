package com.colisweb.jrubysnesshours.jruby

import java.time.ZonedDateTime

import com.colisweb.Approbation
import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule.rubyWeekDayToJavaWeekDay
import com.colisweb.jrubysnesshours.jruby.SampleSchedule.jrubySchedule
import com.github.writethemfirst.approvals.utils.FunctionUtils

import collection.JavaConverters._

class JRubyzScheduleApprobationSpec extends Approbation {

  "jrubySchedule" should "split on 2 weeks" in { approver =>
    val segments = jrubySchedule.splitTimeSegments(
      ZonedDateTime.parse("2019-05-06T11:50:39Z"),
      ZonedDateTime.parse("2019-05-20T16:17:39Z"),
      2
    )

    approver.verify(prettify(segments))
  }

  it should "convert days" in { approver =>
    approver.verify(FunctionUtils.applyCombinations((0 to 6).asJava, rubyWeekDayToJavaWeekDay))
  }
}
