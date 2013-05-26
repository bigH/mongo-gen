package com.bigh.mongogen.ast.documents

import com.bigh.mongogen.ast._

case class Document(statements: Seq[ASTStatement]) extends ASTDocument
object EmptyDocument extends Document(Seq.empty)
