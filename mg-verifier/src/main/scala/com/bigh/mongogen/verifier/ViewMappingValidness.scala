package com.bigh.mongogen.verifier

import com.bigh.mongogen.verifier.api._
import com.bigh.mongogen.ast._
import com.bigh.mongogen.ast.statements._
import com.bigh.mongogen.ast.field._

class ViewMappingValidness extends Verifier {

  def apply(document: ASTDocument): VerificationResult = {
    val entities = document.statements.collect {
      case entity: Entity => entity
    }

    val views = document.statements.collect {
      case view: View => view
    }

    buildResult("View to Entity Mapping Validness",
                verifyViewToEntityMappings(views, entities))
  }

  private def verifyViewToEntityMappings(views: Seq[View],
                                         entities: Seq[Entity]): Seq[String] = {
    val errorOptions = for {
      view <- views
    } yield {
      view.entity match {
        case NonGenericClass(entityName) =>
          entityIn(entities)(entityName) match {
            case Some(entity) =>
              val fields = view.fields
              if (fields.length == 0) {
                Some(s"View `${view.name.name}` has no fields")
              } else if (!onlyIncludes(fields) && !onlyExcludes(fields)) {
                Some(s"View `${view.name.name}` mixes includes and excludes; that is not supported")
              } else {
                buildErrorForMappingIssues(view, entity, entities)
              }
            case None =>
              Some(s"View `${view.name.name}` refers to an entity `$entityName` that was not found")
          }
        case generic =>
          Some(s"View `${view.name.name}` depends on a generic entity; Generics entities are not supported")
      }
    }

    errorOptions collect {
      case Some(error: String) => error
    }
  }

  private def buildErrorForMappingIssues(view: View, entity: Entity, entities: Seq[Entity]): Option[String] = {
    validateProjections(None, view.name.toString, view.fields, entity, entities)
  }

  private def errorIfFieldExists(name: String, entity: Entity, viewName: String): Option[String] = {
    if (fieldIn(entity)(name).isDefined)
      None
    else
      Some(s"Field `$name` in projection `$viewName` not found in entity `${entity.name}`")
  }

  private def validateProjectedSubEntity(prefix: Option[String], fieldName: String, subProjections: Seq[ViewSpec],
                                         viewName: String, entity: Entity, entities: Seq[Entity]): Option[String] = {
    def prefixed(name: String) =
      if (prefix.isEmpty) name
      else prefix.get + "." + name

    val fieldOption = entity.fields find {
      _.name == fieldName
    }

    fieldOption match {
      case Some(Field(name, NonGenericClass(className))) =>
        entityIn(entities)(className) match {
          case Some(subEntity) =>
            validateProjections(Some(prefixed(fieldName)), viewName, subProjections, subEntity, entities)
          case None =>
            Some(s"Field `${prefixed(fieldName)}` in entity `${entity.name}` is of type `$className`; `$className` doesn't exist")
        }
      case Some(Field(name, generic)) =>
        Some(s"Field `${prefixed(fieldName)}` in projection `$viewName` cannot use sub-projections AND be generic")
      case None =>
        Some(s"Field `${prefixed(fieldName)}` in projection `$viewName` not found in entity `${entity.name}`")
    }
  }

  private def validateProjections(prefix: Option[String], viewName: String, specs: Seq[ViewSpec],
                                  entity: Entity, entities: Seq[Entity]): Option[String] = {
    def withFieldName(name: String) =
      if (prefix.isEmpty) name
      else name + "." + prefix.get

    val badFields = specs flatMap {
      spec: ViewSpec =>
        spec.fields map {
          field: FieldRef =>
            field match {
              case CompleteField(name) =>
                errorIfFieldExists(name, entity, viewName)
              case ProjectedField(name, projections) =>
                errorIfFieldExists(name, entity, viewName) orElse
                  validateProjectedSubEntity(prefix, name, projections, viewName, entity, entities)
            }
        }
    }

    val errors = badFields collect {
      case Some(error) => error
    }

    if (errors.isEmpty) None
    else Some (
      s"projection mapping ${withFieldName(viewName)} is not valid\n -> caused by: \n\t - ${
        errors.mkString("\n - ").replaceAll("\\n", "\n\t")
      }"
    )
  }

  private def entityIn(entities: Seq[Entity])(name: String): Option[Entity] = {
    entities find {
      _.name == name
    }
  }

  private def fieldIn(entity: Entity)(name: String): Option[Field] = {
    entity.fields find {
      _.name == name
    }
  }

  private def onlyIncludes(fields: Seq[ViewSpec]): Boolean = {
    fields forall {
      spec: ViewSpec =>
        spec match {
          case IncludeViewSpec(_) => true
          case ExcludeViewSpec(_) => false
        }
    }
  }

  private def onlyExcludes(fields: Seq[ViewSpec]): Boolean = {
    fields forall {
      spec: ViewSpec =>
        spec match {
          case IncludeViewSpec(_) => false
          case ExcludeViewSpec(_) => true
        }
    }
  }
}
