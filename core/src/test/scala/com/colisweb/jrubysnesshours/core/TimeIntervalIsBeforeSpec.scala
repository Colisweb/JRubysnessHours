package com.colisweb.jrubysnesshours.core

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TimeIntervalIsBeforeSpec extends AnyWordSpec with Matchers {
  import SpecUtils._

  /*
                       |__________ 8 ___________|

             |___ 6 ___|------- interval -------|___ 7 ___|

         |__ 1.1 __|                                |__ 1.2 __|
                              |__ 2 __|
                  |__ 3 __|                 |__4 __|

              |__________________ 5 __________________|

   */

  "TimeInterval#isBefore" when {
    val interval = "11:00" - "12:00"

    "1. intervals are not connected" when {
      "1.1 passed interval is before" in {
        val passed = "08:00" - "10:00"
        interval isBefore passed shouldBe false
      }
      "1.2 passed interval is after" in {
        val passed = "13:00" - "14:00"
        interval isBefore passed shouldBe true
      }
    }
    "2. passed interval is enclosed in" in {
      val passed = "11:30" - "11:45"
      interval isBefore passed shouldBe false
    }
    "3. passed interval overlap the 'start'" in {
      val passed = "10:00" - "11:42"
      interval isBefore passed shouldBe false
    }
    "4. passed interval overlap the 'end'" in {
      val passed = "11:47" - "12:22"
      interval isBefore passed shouldBe false
    }
    "5. passed interval overlap all the interval" in {
      val passed = "10:00" - "13:00"
      interval isBefore passed shouldBe false
    }
    "6. passed interval abuts the 'start'" in {
      val passed = "10:00" - "11:00"
      interval isBefore passed shouldBe false
    }
    "7. passed interval abuts the 'end'" in {
      val passed = "12:00" - "13:00"
      interval isBefore passed shouldBe true
    }
    "8. passed interval is the same" in {
      interval isBefore interval shouldBe false
    }
  }
}
