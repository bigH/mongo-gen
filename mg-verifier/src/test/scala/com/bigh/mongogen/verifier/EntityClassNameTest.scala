package com.bigh.mongogen.verifier

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import com.bigh.mongogen.verifier.helpers.VerifierTestHelper
import com.bigh.mongogen.ast.field._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.documents._
import scala.util.DynamicVariable

class EntityClassNameTest extends FreeSpec with ShouldMatchers with VerifierTestHelper {

  val it = new EntityClassName

  "The Entity Class Name verifier" - {
    "should not allow" - {
      "unknown types" in {
        it should fail (NonGenericClass("Unknown"))
      }

      "empty generic types" in {
        it should fail (GenericClass("List", Seq()))
        it should fail (GenericClass("Map", Seq()))
      }

      "generic types with incorrect number of parameters" in {
        it should fail (GenericClass("List", Seq(NonGenericClass("Int"), NonGenericClass("String"))))
        it should fail (GenericClass("Map", Seq(NonGenericClass("Int"))))
      }

      "unknown generic types" in {
        it should fail (GenericClass("Unkown", Seq()))
        it should fail (GenericClass("Unkown", Seq(NonGenericClass("Int"))))
      }

      "unknown types in generics" in {
        it should fail (GenericClass("List", Seq(NonGenericClass("Unknown"))))
        it should fail (GenericClass("Map", Seq(NonGenericClass("Int"), NonGenericClass("Unknown"))))
        it should fail (GenericClass("Map", Seq(NonGenericClass("Unknown"), NonGenericClass("Int"))))
      }

      "unknown types nested in generics" in {
        it should fail (GenericClass("List", Seq(GenericClass("List", Seq(NonGenericClass("Unknown"))))))
      }

      "repeat entity types" in {
        withCustomType("Custom") {
          withCustomType("Custom") {
            it should fail(Document(entities.value))
          }
        }
      }
    }

    "should allow" - {
      "an empty document" in {
        it should pass (EmptyDocument)
      }

      "primitive types" in {
        it should pass (NonGenericClass("Int"))
        it should pass (NonGenericClass("Long"))
        it should pass (NonGenericClass("Float"))
        it should pass (NonGenericClass("Double"))
        it should pass (NonGenericClass("String"))
      }

      "custom types" in {
        withCustomType("Custom") {
          it should pass (NonGenericClass("Custom"))
        }
      }

      "container types with primitives" in {
        it should pass (GenericClass("Option", Seq(NonGenericClass("Int"))))
        it should pass (GenericClass("List", Seq(NonGenericClass("String"))))
        it should pass (GenericClass("Seq", Seq(NonGenericClass("Long"))))
      }

      "container types with custom types" in {
        withCustomType("Custom") {
          it should pass (GenericClass("Option", Seq(NonGenericClass("Custom"))))
        }
      }

      "mapper types with primitives" in {
        it should pass (GenericClass("Map", Seq(NonGenericClass("Int"), NonGenericClass("String"))))
      }

      "mapper types with custom types" in {
        withCustomType("Custom") {
          it should pass (GenericClass("Map", Seq(NonGenericClass("Int"), NonGenericClass("Custom"))))
          it should pass (GenericClass("Map", Seq(NonGenericClass("Custom"), NonGenericClass("String"))))
        }
      }

      "deeply nested generic types" in {
        withCustomType("Custom") {
          it should pass (
            GenericClass (
              "Map",
              Seq (
                GenericClass("Option", Seq(NonGenericClass("Custom"))),
                GenericClass("List", Seq(NonGenericClass("String")))
              )
            )
          )
        }
      }
    }
  }

  private val entities = new DynamicVariable[Seq[Entity]](Seq.empty)

  private def withCustomType(name: String)(thunk: => Unit) {
    entities.withValue(entities.value ++ Seq(Entity(EntityType.Primary, name, Seq.empty)))(thunk)
  }

  import scala.language.implicitConversions
  private implicit def fieldToDocumentImplicit(classDef: ClassDef): Document =
    Document(
      entities.value ++
      Seq(
        Entity(EntityType.Primary, "test", Seq(Field("field", classDef)))
      )
    )

}
