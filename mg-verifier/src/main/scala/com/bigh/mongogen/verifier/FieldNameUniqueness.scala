package com.bigh.mongogen.verifier

import com.bigh.mongogen.verifier.api._
import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field._

class FieldNameUniqueness extends Verifier {
  def apply(document: ASTDocument): VerificationResult = {
    val entities = document.statements.collect {
      case entity: Entity => entity
    }

    val errors = entities flatMap {
      entity: Entity =>
        val fields = entity.elements collect {
          case f: Field => f
        }

        for {
          (name, fields) <- fields.groupBy(_.name)
          if fields.length > 1
        } yield {
          s"Entity ${entity.name} with has more than one field with name $name"
        }
    }

    buildResult("Field Name Uniqueness", errors)
  }
}
