package com.bigh.mongogen.parser

import org.specs2.mutable.Specification
import com.bigh.mongogen.ast.documents._
import com.bigh.mongogen.ast.statements._


class MongoGenFileParserTest extends Specification with ParsingTestHelpers{
  val parsers = new MongoGenFileParser
  "Parsers" should {
    "be able to parse an empty document" in {
      parsers.parseString("") should be equalTo(EmptyDocument)
    }

    "be able to parse an empty document with only comments" in {
      parsers.parseString(
        "/** hello **/" ~
        "// comments // comments" ~
        ""
      ) should be equalTo(EmptyDocument)
    }

    "be able to parse a single statement document" in {
      parsers.parseString("package ab.c") should be equalTo(Document (
        PackageStatement("ab.c") :: Nil
      ))
    }

    "be able to parse a single statement document with comments" in {
      parsers.parseString(
        "/**" ~
        "break stuff " ~
        "*/package /*intersperse //inner comments" ~
        "*/ab.c//hell/*o//hello*/"
      ) should be equalTo(Document (
        PackageStatement("ab.c") :: Nil
      ))
    }

    "be able to parse a basic document" in {
      val document =
        "package ab.c" ~
        "package abbbb.sc"

      parsers.parseString(document) should be equalTo (
        Document (
          PackageStatement("ab.c") ::
          PackageStatement("abbbb.sc") :: Nil
        )
      )
    }

    "be able to parse a basic document with whitespace" in {
      val document =
        "  " ~
        "  package ab.c" ~
        "  " ~
        "package abbbb.sc  " ~
        " "

      parsers.parseString(document) should be equalTo (
        Document (
          PackageStatement("ab.c") ::
          PackageStatement("abbbb.sc") :: Nil
        )
      )
    }
  }
}
