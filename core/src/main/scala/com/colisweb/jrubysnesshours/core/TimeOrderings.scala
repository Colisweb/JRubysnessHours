package com.colisweb.jrubysnesshours.core

import java.time.{LocalDate, LocalDateTime}

object TimeOrderings {

  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

  implicit val localDateOrdering: Ordering[LocalDate] = (x: LocalDate, y: LocalDate) => x.compareTo(y)

}
