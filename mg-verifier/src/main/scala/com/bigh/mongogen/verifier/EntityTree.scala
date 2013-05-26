package com.bigh.mongogen.verifier

import com.bigh.mongogen.ast.ASTDocument
import com.bigh.mongogen.verifier.api.{Verified, VerificationResult, VerificationFailed, Verifier}
import com.bigh.mongogen.ast.statements.{EntityType, Entity}
import com.bigh.mongogen.ast.field.{ClassDef, Field, NonGenericClass}

class EntityTree extends Verifier {

  def apply(document: ASTDocument) = {
    val entities = document.statements collect {
      case entity: Entity => entity
    }
    val primaryEntities = entities collect {
      case entity @ Entity(EntityType.Primary, _, _, _) => entity
    }

    if (primaryEntities.size == 0) {
      VerificationFailed("Entity Tree Verification step failed. No primary entities found.")
    } else if (primaryEntities.size > 1) {
      VerificationFailed(s"Entity Tree Verification step failed. More than one primary entity found:\n\t - ${
        primaryEntities map { _.name } mkString ("\n\t - ")
      }")
    } else {
      verifyNoUnused(entities) &&
      verifyNoRecursion(entities)
    }
  }

  private def verifyNoUnused(entities: Seq[Entity]): VerificationResult = {
    def isPrimary(entity: Entity): Boolean = entity.entityType == EntityType.Primary

    def isUsed(entity: Entity): Boolean =
      entities exists {
        _.fields exists {
          _.classDef == NonGenericClass(entity.name)
        }
      }

    val errors = entities map {
      entity: Entity =>
        (entity, !isPrimary(entity) && !isUsed(entity))
    } collect {
      case (entity, true) =>
        s"non-primary entity `${entity.name}` is not referenced in any other entity"
    }

    buildResult("Entity Recursion", errors)
  }

  private def verifyNoRecursion(entities: Seq[Entity]): VerificationResult = {
    def routesToSelf(entity: Entity): Seq[String] = {
      routesToEntity(source = entity, to = entity, entities = entities)
    }

    val errors = entities map {
      entity: Entity =>
        (entity, routesToSelf(entity))
    } collect {
      case (entity, routes) if (!routes.isEmpty) =>
        s"entity `${entity.name}` is able to create an infinite object loop through these routes:\n\t - ${
          routes.mkString("\n\t - ")
        }"
    }

    buildResult("Entity Recursion", errors)
  }

  private def routesToEntity(source: Entity, to: Entity, entities: Seq[Entity]): Seq[String] = {
    def entityFor(classDef: ClassDef): Option[Entity] = {
      entities.find {
        entity: Entity =>
          NonGenericClass(entity.name) == classDef
      }
    }

    def routesToEntityFrom(from: Entity, entitiesVisited: Seq[Entity]): Either[Seq[String], Boolean] = {
      if (entitiesVisited contains from) {
        Right(true)
      } else {
        val routes = from.fields flatMap {
          field: Field =>
            entityFor(field.classDef) match {
              case Some(entity) =>
                routesToEntityFrom(entity, Seq(from) ++ entitiesVisited) match {
                  case Right(false) => Seq.empty
                  case Right(true) => Seq(field.name)
                  case Left(routes) => routes map { route: String => field.name + "." + route }
                }
              case None =>
                Seq.empty
            }
        }
        if (routes.isEmpty)
          Right(false)
        else
          Left(routes)
      }
    }

    routesToEntityFrom(source, Seq.empty) match {
      case Left(routes) => routes
      case Right(false) => Seq.empty
      case Right(true) =>
        throw new Exception("Did not expect Right(true); entry point of recursion means Seq.empty visits")
    }
  }

}
