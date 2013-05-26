import sbt._

object Modules extends Build {
  lazy val root = RootModule.project

  lazy val mgCommon = MongoGenCommon.project
  lazy val mgParser = MongoGenParser.project
  lazy val mgVerifier = MongoGenVerifier.project
  lazy val mgGeneratorApi = MongoGenGeneratorApi.project
  lazy val mgCasbahGenerator = MongoGenCasbahGenerator.project
  lazy val mgAssembly = MongoGenAssembly.project
}
