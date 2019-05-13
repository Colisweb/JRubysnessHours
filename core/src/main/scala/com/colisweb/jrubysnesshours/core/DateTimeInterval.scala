package com.colisweb.jrubysnesshours.core

import java.time.LocalDateTime

import com.colisweb.jrubysnesshours.core.utils.Orderings._

import scala.math.Ordering.Implicits._

final case class DateTimeInterval(start: LocalDateTime, end: LocalDateTime) {
  assert(start < end, s"DateTimeInterval error: 'start' ($start) is after 'end' ($end)")
}
