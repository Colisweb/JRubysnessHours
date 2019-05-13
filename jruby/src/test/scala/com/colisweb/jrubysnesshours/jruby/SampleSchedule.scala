package com.colisweb.jrubysnesshours.jruby

import com.colisweb.jrubysnesshours.jruby.JRubyzSchedule.{exception, planningEntry, schedule}

object SampleSchedule {
  val sixDays12_18 = Array(
    planningEntry(1, "12:00", "18:00"),
    planningEntry(2, "12:00", "18:00"),
    planningEntry(3, "12:00", "18:00"),
    planningEntry(4, "12:00", "18:00"),
    planningEntry(5, "12:00", "18:00"),
    planningEntry(6, "12:00", "18:00"),
  )

  val jrubySchedule: JRubyzSchedule =
    schedule(
      sixDays12_18,
      Array(exception("2019-05-08T14:00:00Z", "2019-05-08T18:00:00Z")),
      "UTC"
    )
}
