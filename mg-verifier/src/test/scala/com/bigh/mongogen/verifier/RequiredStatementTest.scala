package com.bigh.mongogen.verifier

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import com.bigh.mongogen.ast.documents._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field.NonGenericClass
import com.bigh.mongogen.verifier.helpers.VerifierTestHelper

class RequiredStatementTest extends FreeSpec with ShouldMatchers with VerifierTestHelper {

  val it = new RequiredStatement

  "The Required Statement verifier" - {
    val oneOfEverythingRequired = Seq(
      PackageStatement("first"),
      CollectionName("first"),
      QueryObjectName("first"),
      DataAccessObjectName("first")
    )

    "should allow" - {
      "minimal documents" in {
        it should pass(Document(
          oneOfEverythingRequired
        ))
      }

      "documents with many entity statements" in {
        it should pass(Document(
          Seq(Entity(EntityType.Primary, "alpha", Seq.empty),
              Entity(EntityType.Normal, "beta", Seq.empty)) ++
            oneOfEverythingRequired
        ))
      }

      "documents with many projection statements" in {
        it should pass(Document(
          Seq(View(ViewName("Alpha", true), NonGenericClass("Class"), None, Seq.empty),
              View(ViewName("Beta", false), NonGenericClass("Class"), None, Seq.empty)) ++
            oneOfEverythingRequired
        ))
      }
    }

    "should not allow" - {
      "missing statments" in {
        it should fail(EmptyDocument)
      }

      "mutliple package statments" in {
        it should fail(Document(
          Seq(PackageStatement("test")) ++ oneOfEverythingRequired
        ))
      }

      "mutliple collection statments" in {
        it should fail(Document(
          Seq(CollectionName("test")) ++ oneOfEverythingRequired
        ))
      }

      "mutliple query object statments" in {
        it should fail(Document(
          Seq(QueryObjectName("test")) ++ oneOfEverythingRequired
        ))
      }

      "mutliple data access object statments" in {
        it should fail(Document(
          Seq(DataAccessObjectName("test")) ++ oneOfEverythingRequired
        ))
      }
    }
  }

}
