package com.bigh.mongogen.parser

import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification
import com.bigh.mongogen.ast.documents._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field._
import scala.Some

class MongoGenParsersTest extends Specification with ParserMatchers with ParsingTestHelpers {
  val parsers = new MongoGenParsers

  "the Document parser" should {
    val parser = parsers.document

    "parse empty documents" in {
      parser must (succeedOn("") partially)
      parser must (succeedOn("" ~ "" ~ "") partially)
    }

    "partially parse empty documents" in {
      parser must (succeedOn("" ~ "  " ~ "") partially)
      parser must (succeedOn("" ~ "  " ~ " ") partially)
      parser must (succeedOn(" " ~ "  " ~ "") partially)
      parser must (succeedOn("   " ~ "  " ~ " ") partially)
    }

    "parse single-statement documents" in {
      parser must (
        succeedOn("package a.b") withResult
          Document(PackageStatement("a.b") :: Nil))
    }

    "partially parse single-statement documents" in {
      parser must (
        succeedOn(" " ~
                  "  package a.b " ~
                  " ") partially)
    }

    "parse multi-statement documents" in {
      parser must (
        succeedOn("package a.b" ~
                  "package d.e") partially)
    }

    "partially parse multi-statement documents" in {
      parser must (
        succeedOn(" " ~
                  " package a.b " ~
                  " " ~
                  "  package d.e ") partially)
    }
  }

  "the Package Statement parser" should {
    val parser = parsers.packageStatement

    "parse variations of valid statemements" in {
      parser must (
        succeedOn("package com.some")
          withResult PackageStatement("com.some")
      )
    }

    "NOT parse bad statemements" in {
      parser must (failOn("package com. some"))
      parser must (failOn("package com .some"))
      parser must (failOn("package com some"))
    }
  }

  "the Mongo Collection Statement parser" should {
    val parser = parsers.mongoCollection

    "parse variations of valid statemements" in {
      parser must (
        succeedOn("mongo collection SomeClass")
          withResult CollectionName("SomeClass")
      )
    }

    "NOT parse bad statemements" in {
      parser must (failOn("mongo collection some.stuff"))
      parser must (failOn("mongo collection some stuff"))
    }
  }

  "the Query Object Statement parser" should {
    val parser = parsers.queryObject

    "parse variations of valid statemements" in {
      parser must (
        succeedOn("query object SomeClass")
          withResult QueryObjectName("SomeClass")
      )
    }

    "NOT parse bad statemements" in {
      parser must (failOn("query object some.stuff"))
      parser must (failOn("query object some stuff"))
    }
  }

  "the Data Access Object Statement parser" should {
    val parser = parsers.dataAccessObject

    "parse variations of valid statemements" in {
      parser must (
        succeedOn("data access object SomeClass")
          withResult DataAccessObjectName("SomeClass")
      )
    }

    "NOT parse bad statemements" in {
      parser must (failOn("data access object some.stuff"))
      parser must (failOn("data access object some stuff"))
    }
  }

  "the Entity Type parser" should {
    val parser = parsers.entityType

    "parse primary variation" in {
      parser must (
        succeedOn("primary entity")
          withResult EntityType.Primary
      )
    }

    "parse normal variation" in {
      parser must (
        succeedOn("entity")
          withResult EntityType.Normal
      )
    }

    "NOT parse bad statemements" in {
      parser must (failOn("other entity"))
      parser must (failOn("entity other"))
    }
  }

  "the Value parser" should {
    val parser = parsers.value

    "parse a double" in {
      parser must (succeedOn("0.0") withResult 0d)
      parser must (succeedOn("-0.3") withResult -0.3d)
    }

    "parse an int" in {
      parser must (succeedOn("1") withResult 1)
      parser must (succeedOn("-1") withResult -1)
    }

    "parse a string" in {
      parser must (succeedOn("\"abc\"") withResult "abc")
    }
  }

  "the Require parser" should {
    val parser = parsers.require

    "parse a require statement" in {
      parser must (succeedOn("require (a == 0.0)") withResult Require("a", 0d))
    }
  }

  "the Field parser" should {
    val parser = parsers.field

    "parse basic field" in {
      parser must (
        succeedOn("field : Class") withResult
          Field("field", NonGenericClass("Class"))
        )
    }

    "parse singly generified field" in {
      parser must (
        succeedOn("field : Class[Generic]") withResult
          Field("field", GenericClass("Class",
            Seq(NonGenericClass("Generic"))))
        )
    }

    "parse nested generified field" in {
      parser must (
        succeedOn("field : Class[Generic[SubGeneric]]") withResult
          Field("field", GenericClass("Class",
            Seq(GenericClass("Generic",
              Seq(NonGenericClass("SubGeneric"))))))
        )
    }

    "parse multiply generified field" in {
      parser must (
        succeedOn("field : Class[Generic1, Generic2]") withResult
          Field("field", GenericClass("Class",
            Seq(NonGenericClass("Generic1"),
              NonGenericClass("Generic2"))))
        )
    }

    "NOT parse empty generics" in {
      parser must (failOn("field: Class[]"))
      parser must (failOn("field: Class[Generic[]]"))
    }
  }

  "the Entity Elements parser" should {
    val parser = parsers.entityElements

    "parse a multiple fields with funky whitespace" in {
      parser must (
        succeedOn(
          "  field: " ~ " Class" ~
          "  field2 " ~ " : Class2"
        ) withResult
          Seq(
            Field("field", NonGenericClass("Class")),
            Field("field2", NonGenericClass("Class2"))
          )
        )
    }
  }

  "the Entity Element parser" should {
    val parser = parsers.entityElements

    "parse a field" in {
      parser must (
        succeedOn(
          "field: Class"
        ) withResult
          Seq(
            Field("field", NonGenericClass("Class"))
          )
        )
    }

    "parse a require statement" in {
      parser must (
        succeedOn(
          "require(field == 0)"
        ) withResult
          Seq(
            Require("field", 0)
          )
        )
    }
  }

  "the Entity Statement parser" should {
    val parser = parsers.entity

    "parse a complete entity definition" in {
      parser must (
        succeedOn(
          "primary entity SomeEntity {" ~
          """  require(a == "b")""" ~
          "  field: Class" ~
          "  field2: Class2" ~
          "}"
        ) withResult
          Entity(
            EntityType.Primary,
            "SomeEntity",
            Seq(
              Require("a", "b"),
              Field("field", NonGenericClass("Class")),
              Field("field2", NonGenericClass("Class2"))
            )
          )
      )
    }

    "parse a complete entity definition with inheritance" in {
      parser must (
        succeedOn(
          "primary entity SomeEntity extends OtherEntity {" ~
          "  field: Class" ~
          "  field2: Class2" ~
          "}"
        ) withResult
          Entity(
            EntityType.Primary,
            "SomeEntity",
            Seq(
              Field("field", NonGenericClass("Class")),
              Field("field2", NonGenericClass("Class2"))
            ),
            Some(NonGenericClass("OtherEntity"))
          )
      )
    }
  }

  "the View Name parser" should {
    val parser = parsers.viewName

    "parse with a prefix" in {
      parser must (succeedOn("Prefix*") withResult ViewName("Prefix", true))
      parser must (succeedOn("Prefix *") withResult ViewName("Prefix", true))
    }

    "parse without a prefix" in {
      parser must (succeedOn("PrefixOnClass") withResult ViewName("PrefixOnClass", false))
    }
  }

  "the Field Ref parser" should {
    val parser = parsers.fieldRef

    "parse a field name" in {
      parser must (succeedOn("field") withResult CompleteField("field"))
    }

    "parse a field with projections" in {
      parser must (
        succeedOn("field {" ~
                  "  include inner" ~
                  "}"
      ) withResult ProjectedField("field", Seq(
        IncludeViewSpec(Seq(CompleteField("inner")))
      )))
    }

    "NOT parse a weird field" in {
      parser must (failOn("field { }"))
      parser must (failOn(""))
    }
  }

  "the Field Refs parser" should {
    val parser = parsers.fieldRefs

    "parse a mixed list of field projections" in {
      parser must (
        succeedOn("field1, field2 {" ~
                  "  include inner" ~
                  "}"
        ) withResult Seq(
          CompleteField("field1"),
          ProjectedField("field2", Seq(
            IncludeViewSpec(Seq(CompleteField("inner")))
          ))
        )
      )
    }

    "NOT parse a weird field" in {
      parser must (failOn("field fieldNotParsed"))
      parser must (failOn(""))
    }
  }

  "the Include parser" should {
    val parser = parsers.includes

    "parse a list of fields" in {
      parser must (
        succeedOn("include field1, field2") withResult
        IncludeViewSpec(Seq(
          CompleteField("field1"),
          CompleteField("field2")
        ))
      )
    }
  }

  "the Exclude parser" should {
    val parser = parsers.excludes

    "parse a list of fields" in {
      parser must (
        succeedOn("exclude field1, field2") withResult
        ExcludeViewSpec(Seq(
          CompleteField("field1"),
          CompleteField("field2")
        ))
      )
    }
  }

  "the View parser" should {
    val parser = parsers.view

    "parse a view with fields" in {
      parser must (
        succeedOn(
          "view Something of SomethingElse {" ~
          "  include a, c {" ~
          "    include g" ~
          "    exclude b" ~
          "  }" ~
          "  exclude d, f {" ~
          "    include g" ~
          "    exclude h" ~
          "  }" ~
          "}"
        ) withResult (
          View (
            ViewName("Something", false),
            NonGenericClass("SomethingElse"),
            None,
            Seq (
              IncludeViewSpec (
                Seq (
                  CompleteField("a"),
                  ProjectedField("c", Seq(
                    IncludeViewSpec(Seq(CompleteField("g"))),
                    ExcludeViewSpec(Seq(CompleteField("b")))
                  ))
                )
              ),
              ExcludeViewSpec (
                Seq (
                  CompleteField("d"),
                  ProjectedField("f", Seq(
                    IncludeViewSpec(Seq(CompleteField("g"))),
                    ExcludeViewSpec(Seq(CompleteField("h")))
                  ))
                )
              )
            )
          )
        )
      )
    }

    "parse a * view with inheritance" in {
      parser must (
        succeedOn(
          "view Something* of SomethingElse extends AnotherThing* {" ~
          "  include a" ~
          "}"
        ) withResult (
          View (
            ViewName("Something", true),
            NonGenericClass("SomethingElse"),
            Some(ViewName("AnotherThing", true)),
            Seq (
              IncludeViewSpec (
                Seq (
                  CompleteField("a")
                )
              )
            )
          )
        )
      )
    }
  }
}
