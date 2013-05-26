package com.bigh.mongogen.ast

import scala.util.parsing.input.Positional

trait ASTFieldElement extends Positional
trait ASTStatement extends Positional
trait ASTDocument extends Positional {
  def statements: Seq[ASTStatement]
}
