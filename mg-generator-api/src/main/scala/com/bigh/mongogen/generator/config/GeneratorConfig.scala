package com.bigh.mongogen.generator.config

import java.io.File
import scala.util.matching.Regex

case class GeneratorConfig (
  output: OutputConfiguration,
  input: InputConfiguration
)

case class OutputConfiguration (
  directory: File
) {
  require(directory.exists(),
          s"Directory in OutputConfiguration ${directory.getAbsolutePath} doesn't exist.")
  require(directory.isDirectory(),
          s"Directory in OutputConfiguration ${directory.getAbsolutePath} isn't a directory.")
}

case class InputConfiguration (
  directory: File,
  recursive: Boolean = true,
  regex: Regex = ".*".r
) {
  require(directory.exists(),
          s"Directory in InputConfiguration ${directory.getAbsolutePath} doesn't exist.")
  require(directory.isDirectory(),
          s"Directory in InputConfiguration ${directory.getAbsolutePath} isn't a directory.")
  require(!directory.list().isEmpty,
          s"Directory in InputConfiguration ${directory.getAbsolutePath} contains nothing.")
}
