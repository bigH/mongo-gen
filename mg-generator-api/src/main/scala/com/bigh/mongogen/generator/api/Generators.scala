package com.bigh.mongogen.generator.api

import com.bigh.mongogen.generator.config.GeneratorConfig

class Generators(generators: Seq[Generator]) extends Generator {
  def generate(config: GeneratorConfig) {
    generators map {
      _.generate(config)
    } reduce {
      _ && _
    }
  }
}

trait Generator {
  def generate(config: GeneratorConfig): GenerationResult
}

trait Issue
case class Warning(message: String) extends Issue
case class Error(message: String) extends Issue

sealed trait GenerationResult {
  def isSuccess: Boolean
  def ||(right: => GenerationResult): GenerationResult
  def &&(right: => GenerationResult): GenerationResult
}

case class GenerationSuccess(issues: Seq[Warning] = None) extends GenerationResult {
  def isSuccess = true
  def ||(right: => GenerationResult) = this
  def &&(right: => GenerationResult) = right
}

case class GenerationFailure(error: Seq[Issue]) extends GenerationResult {
  def isSuccess = false
  def ||(right: => GenerationResult) = right
  def &&(right: => GenerationResult) = this
}