package com.bigh.mongogen.ast.statements

import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.field._

case class PackageStatement(packageName: String) extends ASTStatement

case class CollectionName(name: String) extends ASTStatement

case class QueryObjectName(name: String) extends ASTStatement
case class DataAccessObjectName(name: String) extends ASTStatement

object EntityType extends Enumeration {
  val Primary, Normal = Value
}

case class Entity(entityType: EntityType.Value,
                  name: String,
                  elements: Seq[EntityElement],
                  superType: Option[ClassDef] = None) extends ASTStatement {

  def fields: Seq[Field] = elements.collect {
    case f: Field => f
  }

  def requirements: Seq[Require] = elements.collect {
    case r: Require => r
  }
}

case class ViewName(name: String,
                    isPrefix: Boolean) extends ASTStatement {
  override def toString: String =
    name + {
      if (isPrefix) "*"
      else ""
    }
}

case class View(name: ViewName,
                entity: ClassDef,
                extension: Option[ViewName],
                fields: Seq[ViewSpec]) extends ASTStatement
