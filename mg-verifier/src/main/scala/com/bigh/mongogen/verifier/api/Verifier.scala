package com.bigh.mongogen.verifier.api

import com.bigh.mongogen.ast.ASTDocument

trait Verifier {
  def apply(document: ASTDocument): VerificationResult

  def buildResult(step: String, errors: Seq[String]): VerificationResult = {
    if (errors.isEmpty) Verified
    else VerificationFailed (
      s"$step Verification step failed with ${errors.size} errors:\n\n${
        errors.mkString("\n\n").replaceAll("\\n", "\n\t")
      }"
    )
  }
}

trait VerificationResult {
  def isVerified: Boolean
  def ||(right: => VerificationResult): VerificationResult
  def &&(right: => VerificationResult): VerificationResult
}

case object Verified extends VerificationResult {
  def isVerified = true
  def ||(right: => VerificationResult): VerificationResult = this
  def &&(right: => VerificationResult): VerificationResult = right
}

case class VerificationFailed(message: String) extends VerificationResult {
  def isVerified = false
  def ||(right: => VerificationResult): VerificationResult = right
  def &&(right: => VerificationResult): VerificationResult = this
}

