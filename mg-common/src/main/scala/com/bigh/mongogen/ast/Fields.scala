package com.bigh.mongogen.ast.field

import com.bigh.mongogen.ast._

sealed trait EntityElement extends ASTFieldElement

case class Field(name: String, classDef: ClassDef) extends EntityElement
case class Require(name: String, value: Any) extends EntityElement

sealed trait ClassDef extends ASTFieldElement

case class NonGenericClass(name: String) extends ClassDef
case class GenericClass(name: String, genericClasses: Seq[ClassDef]) extends ClassDef

sealed trait FieldRef extends ASTFieldElement {
  def name: String
}

case class CompleteField(name: String) extends FieldRef
case class ProjectedField(name: String, projections: Seq[ViewSpec]) extends FieldRef

sealed trait ViewSpec extends ASTFieldElement {
  def fields: Seq[FieldRef]
}

case class IncludeViewSpec(fields: Seq[FieldRef]) extends ViewSpec
case class ExcludeViewSpec(fields: Seq[FieldRef]) extends ViewSpec
