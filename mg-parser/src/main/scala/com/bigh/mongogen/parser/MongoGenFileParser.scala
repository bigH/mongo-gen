package com.bigh.mongogen.parser

import java.io.{CharArrayReader, FileNotFoundException, File}
import scala.io.Source
import com.bigh.mongogen.ast.ASTDocument

class MongoGenFileParser {
  val parsers = new MongoGenParsers

  def parseString(entityDef: String): ASTDocument = {
    val reader = new CharArrayReader(entityDef.toArray)
    val documentParser = parsers.positioned(parsers.document)
    val result = parsers.parseAll(documentParser, reader)
    result match {
      case parsers.Success(document: ASTDocument, _) => document
      case parsers.Success(document, _) =>
        throw new Exception(s"Could not extract document as an ASTDocument; file a bug")
      case failedParse =>
        throw new Exception(s"Script was not parsed successfully: $failedParse")
    }
  }

  def parseFile(file: File): ASTDocument = {
    val fileAsString = Source.fromFile(file).mkString
    parseString(fileAsString)
  }

  def parseFiles(file1: File, file2: File, moreFiles: File*): Seq[ASTDocument] = {
    val files = Seq(file1, file2) ++ moreFiles
    files map { parseFile(_) }
  }

  def parseDirectory(path: String, recurse: Boolean = false, extension: Option[String] = Some("mgn")): Seq[ASTDocument] = {
    val directory = new File(path)

    val result =
      if (directory.exists) {
        if (directory.isDirectory) {
          parseKnownDirectory(directory, recurse, extension)
        } else {
          throw new FileNotFoundException(s"Directory NOT found at path '$path'; found file")
        }
      } else {
        throw new FileNotFoundException(s"Nothing found at path '$path'")
      }

    if (result.isEmpty) {
      throw new FileNotFoundException(s"Directory '$path' had no files matching extension $extension")
    } else {
      result
    }
  }

  private def parseKnownDirectory(directory: File, recurse: Boolean, extension: Option[String]): Seq[ASTDocument] = {
    val directoryContents = directory.listFiles()
    val (directories, files) = directoryContents partition {
      _.isDirectory
    }

    val directoryResults = if (recurse) {
      directories flatMap {
        parseKnownDirectory(_, recurse, extension)
      }
    } else {
      Array.empty[ASTDocument]
    }

    val filesToProcess = filterFiles(files, extension)
    val fileResults = filesToProcess map { parseFile(_) }

    directoryResults ++ fileResults
  }

  private def filterFiles(files: Array[File], extension: Option[String]) =
    extension match {
      case None => files
      case Some(extension) if (extension startsWith ".") =>
        files filter {
          _.getName.endsWith(extension)
        }
      case Some(extension) =>
        files filter {
          _.getName.endsWith("." + extension)
        }
    }

}
