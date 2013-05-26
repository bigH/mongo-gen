package com.bigh.mongogen.verifier

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import com.bigh.mongogen.verifier.helpers.VerifierTestHelper
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field._
import com.bigh.mongogen.ast.documents._

class EntityTreeTest extends FreeSpec with ShouldMatchers with VerifierTestHelper {

  val it = new EntityTree

  "The Entity Recursion verifier" - {
    "should not allow" - {
      "a document without primary entities" in {
        it should fail (
          Document (
            Entity (EntityType.Normal, "Stuff", Seq ()) :: Nil
          )
        )
      }

      "a document with two primary entities" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq ()) ::
            Entity (EntityType.Primary, "OtherStuff", Seq ()) :: Nil
          )
        )
      }

      "a document with recursive primary entity" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field("a", NonGenericClass("Stuff"))
            )) :: Nil
          )
        )
      }

      "a document with recursive secondary entity" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field("a", NonGenericClass("OtherStuff"))
            )) ::
            Entity (EntityType.Normal, "OtherStuff", Seq (
              Field("a", NonGenericClass("OtherStuff"))
            )) :: Nil
          )
        )
      }

      "a document with multi-entity recursion at primary level" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field("a", NonGenericClass("OtherStuff"))
            )) ::
            Entity (EntityType.Normal, "OtherStuff", Seq (
              Field("a", NonGenericClass("Stuff"))
            )) :: Nil
          )
        )
      }

      "a document with multi-entity recursion at secondary level" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field("a", NonGenericClass("OtherStuff"))
            )) ::
            Entity (EntityType.Normal, "OtherStuff", Seq (
              Field("a", NonGenericClass("AnotherStuff"))
            )) ::
            Entity (EntityType.Normal, "AnotherStuff", Seq (
              Field("a", NonGenericClass("OtherStuff"))
            )) :: Nil
          )
        )
      }
    }

    "should allow" - {
      "a single primary entity" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq ()) :: Nil
          )
        )
      }

      "a deep primary entity" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field("a", NonGenericClass("OtherStuff"))
            )) ::
              Entity (EntityType.Normal, "OtherStuff", Seq (
              Field("a", NonGenericClass("SomeOtherStuff"))
            )) ::
            Entity (EntityType.Normal, "SomeOtherStuff", Seq ()) ::
              Nil
          )
        )
      }
    }
  }

}

