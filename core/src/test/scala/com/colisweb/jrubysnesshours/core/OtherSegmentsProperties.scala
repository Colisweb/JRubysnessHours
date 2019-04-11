package com.colisweb.jrubysnesshours.core

import com.colisweb.jrubysnesshours.core.Generators._
import com.colisweb.jrubysnesshours.core.SpecUtils._
import org.scalacheck.Gen.listOfN
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OtherSegmentsProperties
    extends FlatSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  "excludingSegmentFromAnother" should "be empty when excluding outer from inner" in {
    forAll(genLocalDate, listOfN(4, genLocalTime)) { (date, times) =>
      val List(a, b, c, d) = times.sorted
      excludingSegmentFromAnother(date.ts(b, c), date.ts(a, d)) shouldBe empty
    }
  }

  it should "keep start when excluding end" in {
    forAll(genLocalDate, listOfN(3, genLocalTime)) { (date, times) =>
      val List(a, b, c) = times.sorted
      excludingSegmentFromAnother(date.ts(a, c), date.ts(b, c)) shouldBe List(
        date.ts(a, b)
      )
    }
  }

}
