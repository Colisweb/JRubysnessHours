package com.colisweb.jrubysnesshours.core

import com.colisweb.jrubysnesshours.core.Core.{Interval, TimeSegment}
import com.colisweb.jrubysnesshours.core.Generators._
import com.colisweb.jrubysnesshours.core.Segments.excludingSegmentFromAnother
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

      val ad = TimeSegment(date, Interval(a, d))
      val bc = TimeSegment(date, Interval(b, c))

      excludingSegmentFromAnother(bc, ad) shouldBe empty
    }
  }

  it should "keep start when excluding end" in {
    forAll(genLocalDate, listOfN(3, genLocalTime)) { (date, times) =>
      val List(a, b, c) = times.sorted

      val ac = TimeSegment(date, Interval(a, c))
      val bc = TimeSegment(date, Interval(b, c))
      val ab = TimeSegment(date, Interval(a, b))

      excludingSegmentFromAnother(ac, bc) shouldBe List(ab)
    }
  }

}
