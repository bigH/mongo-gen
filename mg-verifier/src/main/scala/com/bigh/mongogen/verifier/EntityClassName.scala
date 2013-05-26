package com.bigh.mongogen.verifier

import com.bigh.mongogen.verifier.api._
import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field._

class EntityClassName extends Verifier {
  val primitives = Seq("Int", "Long", "Float", "Double", "String")
  val containers = Seq("Seq", "List", "Option")
  val mappers = Seq("Map")

  def apply(document: ASTDocument) = {
    val entities = document.statements.collect {
      case entity: Entity => entity
    }

    val entityNames = document.statements.collect {
      case entity: Entity => entity.name
    }

    validateClassUniqueness(entities) &&
    validateClassesIn(entities, entityNames)
  }

  private def validateClassUniqueness(entities: Seq[Entity]): VerificationResult = {

    val violatingEntities = entities.groupBy(_.name).values.filter(_.size > 1)
    val violatingEntityNames = violatingEntities map (_.head.name)
    val errors = violatingEntityNames map {
      name: String =>
        s"duplicate entities with name `$name` found"
    }

    buildResult("Class Name Uniqueness", errors.toSeq)
  }

  private def validateClassesIn(entities: Seq[Entity],
                                entityNames: Seq[String]): VerificationResult = {

    val errors = entities flatMap {
      (entity: Entity) =>
        isEntityValid(entityNames, entity)
    }

    buildResult("Class Name Mappings", errors)
  }

  private def isEntityValid(entityNames: Seq[String], entity: Entity): Option[String] = {
    val errors = entity.fields flatMap {
      isEntityElementValid(entityNames, _)
    }
    if (errors.isEmpty) None
    else Some (
      s"entity ${entity.name} is not valid\n -> caused by: \n\t - ${
        errors.mkString("\n - ").replaceAll("\\n", "\n\t")
      }"
    )
  }

  private def isEntityElementValid(entityNames: Seq[String], field: Field): Option[String] = {
    val Field(name, classDef) = field
    validateClass(entityNames, classDef) map {
      message: String =>
        s"field ${name} with invalid type $classDef\n\t -> caused by: \n\t - ${message.replaceAll("\\n", "\n\t")}"
    }
  }

  private def validateClass(entityNames: Seq[String], classDef: ClassDef): Option[String] = {
    classDef match {
      case NonGenericClass(name) =>
        isClassKnown(entityNames, name)
      case GenericClass(name, classDefs @ (t :: Nil)) =>
        isContainerClassValid(entityNames, name, t)
      case GenericClass(name, classDefs @ (t :: u :: Nil)) =>
        isMapperClassValid(entityNames, name, t, u)
      case GenericClass(name, Nil) =>
        Some(s"generic class $name used with no type parameters in $classDef")
      case GenericClass(name, _) =>
        Some(s"more than 2 type parameters are not supported in $classDef")
    }
  }

  private def isClassKnown(entityNames: Seq[String], name: String): Option[String] = {
    if ((entityNames contains name) || (primitives contains name)) None
    else Some(s"unknown type $name")
  }

  private def isContainerClassValid(entityNames: Seq[String], name: String, t: ClassDef): Option[String] = {
    if (containers contains name) {
      validateClass(entityNames, t) map {
        message: String =>
          s"container $name used with unknown generic type\n\t -> caused by \n\t - ${
            message.replaceAll("\\n", "\n\t")
          }"
      }
    } else {
      Some(s"unknown type $name")
    }
  }

  private def isMapperClassValid(entityNames: Seq[String], name: String, t: ClassDef, u: ClassDef): Option[String] = {
    if (mappers contains name) {
      validateClass(entityNames, t) map {
        message: String =>
          s"container $name[T,U] used with unknown generic type for T\n\t -> caused by \n\t - ${
            message.replaceAll("\\n", "\n\t")
          }"
      } orElse {
        validateClass(entityNames, u) map {
          message: String =>
            s"container $name[T,U] used with unknown generic type for U\n\t -> caused by \n\t - ${
              message.replaceAll("\\n", "\n\t")
            }"
        }
      }
    } else {
      Some(s"unknown type $name")
    }
  }
}
