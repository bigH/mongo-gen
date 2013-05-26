package com.bigh.mongogen.verifier.api

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.documents._

class VerifierTest extends FreeSpec with ShouldMatchers {
  "The Simple verifier" - {
    val verifier = new Verifier {
      def apply(document: ASTDocument) = Verified
    }

    "should always return the expected value" in {
      verifier(EmptyDocument) should be(Verified)
    }
  }
}
