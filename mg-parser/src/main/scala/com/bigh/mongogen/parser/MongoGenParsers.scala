package com.bigh.mongogen.parser

import scala.util.parsing.combinator.JavaTokenParsers
import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.documents._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field._

class MongoGenParsers extends JavaTokenParsers {
  import EntityType._

  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def document: Parser[Document] = rep(statement) ^^
    { case Nil => EmptyDocument
      case statements => Document(statements) }

  def statement: Parser[ASTStatement] =
    packageStatement |
    mongoCollection |
    queryObject |
    dataAccessObject

  def packageStatement = "package" ~> packageName <~ (";").? ^^
    { case name => PackageStatement(name) }

  def packageName = """([a-z_]{1}[a-z0-9_]*(\.[a-z_]{1}[a-z0-9_]*)*)""".r

  def mongoCollection = "mongo" ~> "collection" ~> ident <~ (";").? ^^
    { case name => CollectionName(name) }

  def queryObject = "query" ~> "object" ~> ident <~ (";").? ^^
    { case name => QueryObjectName(name) }

  def dataAccessObject = "data" ~> "access" ~> "object" ~> ident <~ (";").? ^^
    { case name => DataAccessObjectName(name) }

  def view = "view" ~ viewName ~ "of" ~ nonGenericClassDef ~ ("extends" ~ viewName).? ~ viewBody ^^
    { case _ ~ name ~ _ ~ entity ~ extensionViewOption ~ specs =>
        View(name, entity, extensionViewOption collect {
          case _ ~ extensionView => extensionView
        }, specs)
    }

  def viewName: Parser[ViewName] = ident ~ "*".? ^^
    { case name ~ None => ViewName(name, false)
      case name ~ Some(_) => ViewName(name, true) }

  def viewBody = "{" ~> viewSpecs <~ "}"

  def viewSpecs = rep1(viewSpec)
  def viewSpec = includes | excludes

  def includes = "include" ~> fieldRefs ^^
    { case fields => IncludeViewSpec(fields) }

  def excludes = "exclude" ~> fieldRefs ^^
    { case fields => ExcludeViewSpec(fields) }

  def fieldRefs: Parser[Seq[FieldRef]] = rep1sep(fieldRef, ",")
  def fieldRef: Parser[FieldRef] = ident ~ viewBody.? ^^
    { case name ~ None => CompleteField(name)
      case name ~ Some(subProjections) => ProjectedField(name, subProjections) }

  def entity = entityType ~ ident ~ extension.? ~ entityBody ^^
    { case eType ~ name ~ superType ~ fields => Entity(eType, name, fields, superType) }

  def extension: Parser[ClassDef] = "extends" ~> classDef

  def entityType: Parser[EntityType.Value] = "primary".? ~ "entity" ^^
    {
      case Some("primary") ~ _ => Primary
      case None ~ _ => Normal
    }

  def entityBody = "{" ~> entityElements <~ "}"
  def entityElements = rep1(entityElement)
  def entityElement = field | require

  def require = "require" ~> "(" ~> requireBody <~ ")"

  def requireBody = ident ~ "==" ~ value ^^
    { case fieldName ~ _ ~ fieldValue => Require(fieldName, fieldValue) }

  def value: Parser[Any] = floatingPointNumber ^^ (_.toDouble) |
                           wholeNumber ^^ (_.toLong) |
                           stringLiteral ^^ (_.drop(1).dropRight(1))

  def field = ident ~ ":" ~ classDef ^^
    { case fieldName ~ _ ~ fieldType => Field(fieldName, fieldType) }

  def nonGenericClassDef: Parser[ClassDef] = ident ^^
    { case name => NonGenericClass(name) }

  def classDef: Parser[ClassDef] = ident ~ generic.? ^^
    { case name ~ None => NonGenericClass(name)
      case name ~ Some(typeDefs) => GenericClass(name, typeDefs) }

  def generic = "[" ~> rep1sep(classDef, ",") <~ "]"
}

