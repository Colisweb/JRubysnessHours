package com.colisweb.jrubysnesshours.core

import org.scalatest.{Matchers, WordSpec}

class TimeIntervalIsConnectedSpec extends WordSpec with Matchers {

  import SpecUtils._

  /*

             |___ 6 ___|------- interval -------|___ 7 ___|

         |__ 1.1 __|                                |__ 1.2 __|
                              |__ 2 __|
                  |__ 3 __|                 |__4 __|

              |__________________ 5 __________________|

   */

  "TimeInterval#isConnected" when {
    "true" should {
      "be true" in {
        true shouldBe true
      }
    }

    val interval = "11:00" - "12:00"

    "1. intervals are not connected" when {
      "1.1 passed interval is before" in {
        val passed = "08:00" - "10:00"
        interval isConnected passed shouldBe false
      }
      "1.2 passed interval is after" in {
        val passed = "13:00" - "14:00"
        interval isConnected passed shouldBe false
      }
    }
    "2. passed interval is enclosed in" in {
      val passed = "11:30" - "11:45"
      interval isConnected passed shouldBe true
    }
    "3. passed interval overlap the 'start'" in {
      val passed = "10:00" - "11:42"
      interval isConnected passed shouldBe true
    }
    "4. passed interval overlap the 'end'" in {
      val passed = "11:47" - "12:22"
      interval isConnected passed shouldBe true
    }
    "5. passed interval overlap all the interval" in {
      val passed = "10:00" - "13:00"
      interval isConnected passed shouldBe true
    }
    "6. passed interval abuts the 'start'" in {
      val passed = "10:00" - "11:00"
      interval isConnected passed shouldBe true
    }
    "7. passed interval abuts the 'end'" in {
      val passed = "12:00" - "13:00"
      interval isConnected passed shouldBe true
    }
  }

}
