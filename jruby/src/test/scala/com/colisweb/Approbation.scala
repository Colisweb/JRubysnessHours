package com.colisweb

import com.github.writethemfirst.approvals.approvers.Approver
import org.scalatest.{Matchers, Outcome, fixture}
import pprint.PPrinter
import pprint.PPrinter.BlackWhite

class Approbation extends fixture.FlatSpec with Matchers {

  private val approvals  = (new Approver).testing(getClass)
  val prettify: PPrinter = BlackWhite.copy(defaultHeight = Int.MaxValue)

  override type FixtureParam = Approver

  // see https://stackoverflow.com/questions/14831246/access-scalatest-test-name-from-inside-test
  // and http://www.scalatest.org/user_guide/sharing_fixtures
  override def withFixture(test: OneArgTest): Outcome = {
    val approver = approvals.writeTo(test.name)
    withFixture(test.toNoArgTest(approver))
  }

}
