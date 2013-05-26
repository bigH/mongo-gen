import sbt._
import Keys._
import Modules._

object RootModule {

  lazy val project = Project (
    id = "mongo-gen",
    base = file("."),
    settings = moduleSettings
  ) aggregate (
    mgParser,
    mgVerifier,
    mgGeneratorApi,
    mgCasbahGenerator,
    mgAssembly
  )

  val moduleSettings =
    MyDefaults.settings ++ Seq (
      name := "mongo-gen"
    )
}
