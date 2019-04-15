package com.colisweb.jrubysnesshours.core

import org.scalatest.{Matchers, WordSpec}

class TimeIntervalContainsSpec extends WordSpec with Matchers {

  import SpecUtils._

  /*

             |------- interval -------|
        ^    ^      ^                 ^   ^
        |    |      |                 |   |
        |    |      |                 |   |
        .    .      .                 .   .
        1    2      3                 4   5
   */

  "TimeInterval#contains" when {
    val interval = "11:00" - "12:00"

    "1. The instant is before" in {
      val instant = "10:00".toLocalTime
      interval contains instant shouldBe false
    }
    "2. The instant is at the 'start'" in {
      val instant = "11:00".toLocalTime
      interval contains instant shouldBe true
    }
    "3. The instant is in" in {
      val instant = "11:25".toLocalTime
      interval contains instant shouldBe true
    }
    "4. The instant is at the 'end'" in {
      val instant = "12:00".toLocalTime
      interval contains instant shouldBe false
    }
    "5. The instant is after" in {
      val instant = "13:00".toLocalTime
      interval contains instant shouldBe false
    }
  }

}
