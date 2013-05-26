package com.bigh.mongogen.verifier

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import com.bigh.mongogen.verifier.helpers.VerifierTestHelper
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.documents._
import com.bigh.mongogen.ast.field._

class FieldNameUniquenessTest extends FreeSpec with ShouldMatchers with VerifierTestHelper {

  val it = new FieldNameUniqueness

  "The Field Name Uniqueness verifier" - {
    "should not allow" - {
      "a class with fields of the same name" in {
        it should fail(Document (
          Seq (
            Entity(EntityType.Primary, "Test1", Seq(
              Field("a", NonGenericClass("c")),
              Field("a", NonGenericClass("b"))
            ))
          )
        ))
      }
    }

    "should allow" - {
      "an empty document" in {
        it should pass(EmptyDocument)
      }

      "two classes with fields of the same name" in {
        it should pass(Document (
          Seq (
            Entity(EntityType.Primary, "Test1", Seq(
              Field("a", NonGenericClass("b"))
            )),
            Entity(EntityType.Primary, "Test1", Seq(
              Field("a", NonGenericClass("b"))
            ))
          )
        ))
      }
    }
  }

}
