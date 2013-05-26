package com.bigh.mongogen.verifier.helpers

import com.bigh.mongogen.ast.ASTDocument
import com.bigh.mongogen.verifier.api.Verifier
import org.scalatest.matchers.{Matcher, MatchResult}

trait VerifierTestHelper {
  def name = it.getClass.getSimpleName

  def it: Verifier

  def pass(document: ASTDocument): Matcher[Verifier] = {
    new Matcher[Verifier] {
      def apply(left: Verifier) = {
        val result = it(document)
        MatchResult(
          result.isVerified,
          s"did not pass $document got verification result $result",
          s"passed $document"
        )
      }
    }
  }

  def fail(document: ASTDocument) = {
    new Matcher[Verifier] {
      def apply(left: Verifier) = {
        val result = it(document)
        MatchResult(
          !result.isVerified,
          s"did not fail $document",
          s"failed $document"
        )
      }
    }
  }

}
