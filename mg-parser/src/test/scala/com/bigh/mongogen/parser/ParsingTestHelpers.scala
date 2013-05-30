package com.bigh.mongogen.parser

trait ParsingTestHelpers {
  implicit class StringWithNewLineAppend(start: String) {
    def ~(end: String): String = start + "\n" + end
  }
}