package com.bigh.mongogen.verifier

import com.bigh.mongogen.verifier.helpers.VerifierTestHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FreeSpec
import com.bigh.mongogen.ast.field._
import com.bigh.mongogen.ast.documents._
import com.bigh.mongogen.ast.statements._

class ViewMappingValidnessTest extends FreeSpec with ShouldMatchers with VerifierTestHelper {

  val it = new ViewMappingValidness

  "The View Mapping Validness verifier" - {
    "should not allow" - {
      "unmatched entity" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("WrongStuff"), None, Seq (
              IncludeViewSpec(Seq (
                CompleteField("a"),
                CompleteField("b")
              ))
            )) :: Nil
          )
        )
      }

      "projection of primitive type" in {
        it should fail (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("WrongStuff"), None, Seq (
              IncludeViewSpec(Seq (
                CompleteField("a"),
                ProjectedField("b", Seq())
              ))
            )) :: Nil
          )
        )
      }
    }

    "should allow" - {
      "a simple valid view with one include" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("Stuff"), None, Seq (
              IncludeViewSpec(Seq (
                CompleteField("a"),
                CompleteField("b")
              ))
            )) :: Nil
          )
        )
      }

      "a simple valid view with more than one include" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("Stuff"), None, Seq (
              IncludeViewSpec(Seq (
                CompleteField("a")
              )),
              IncludeViewSpec(Seq (
                CompleteField("b")
              ))
            )) :: Nil
          )
        )
      }

      "a simple valid view with one exclude" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("Stuff"), None, Seq (
              ExcludeViewSpec(Seq (
                CompleteField("a"),
                CompleteField("b")
              ))
            )) :: Nil
          )
        )
      }

      "a simple valid view with more than one exclude" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("Stuff"), None, Seq (
              ExcludeViewSpec(Seq (
                CompleteField("a")
              )),
              ExcludeViewSpec(Seq (
                CompleteField("b")
              ))
            )) :: Nil
          )
        )
      }

      "a simple view with complex entity" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("SubStuff"))
            )) ::
            Entity (EntityType.Normal, "SubStuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("Stuff"), None, Seq (
              IncludeViewSpec(Seq (
                CompleteField("a"),
                CompleteField("b")
              ))
            )) :: Nil
          )
        )
      }

      "a complex view with complex entity" in {
        it should pass (
          Document (
            Entity (EntityType.Primary, "Stuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("SubStuff"))
            )) ::
            Entity (EntityType.Normal, "SubStuff", Seq (
              Field ("a", NonGenericClass("SubSubStuff")),
              Field ("b", NonGenericClass("SubSubStuff"))
            )) ::
            Entity (EntityType.Normal, "SubSubStuff", Seq (
              Field ("a", NonGenericClass("Int")),
              Field ("b", NonGenericClass("String"))
            )) ::
            View (ViewName("Projected", true), NonGenericClass("Stuff"), None, Seq (
              IncludeViewSpec(Seq (
                CompleteField("a"),
                ProjectedField("b", Seq(
                  IncludeViewSpec(Seq (
                    CompleteField("a"),
                    ProjectedField("b", Seq(
                      IncludeViewSpec(Seq (
                        CompleteField("a"),
                        CompleteField("b")
                      ))
                    ))
                  ))
                ))
              ))
            )) :: Nil
          )
        )
      }
    }
  }

}
