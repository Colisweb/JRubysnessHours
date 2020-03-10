package com.colisweb.jrubysnesshours.core.utils

import java.time.{LocalDate, LocalDateTime}

object Orderings {
  implicit val localDateOrdering: Ordering[LocalDate]         = _ compareTo _
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = _ compareTo _
}
