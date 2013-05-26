package com.bigh.mongogen.verifier

import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.verifier.api._

class RequiredStatement extends Verifier {

  def apply(in: ASTDocument): VerificationResult = {
    implicit val document = in
    checkForExactlyOne("`package` statement", _.isInstanceOf[PackageStatement]) &&
    checkForExactlyOne("`collection` statement", _.isInstanceOf[CollectionName]) &&
    checkForExactlyOne("`query object` statement", _.isInstanceOf[QueryObjectName]) &&
    checkForExactlyOne("`data access object` statement", _.isInstanceOf[DataAccessObjectName])
  }

  private def checkForExactlyOne(name: String, f: ASTStatement => Boolean)
                                (implicit document: ASTDocument): VerificationResult = {

    val filteredSize = document.statements.filter(f).size

    if (filteredSize == 0) {
      VerificationFailed(s"Expected exactly one $name")
    } else if (filteredSize == 1) {
      Verified
    } else {
      VerificationFailed(s"Expected exactly one $name, got more than one")
    }
  }

}
