import sbt._

object Libraries {

  object main {
    def slf4jModule(name: String) = "org.slf4j" % name % Versions.slf4j

    lazy val scopt = "com.github.scopt" %% "scopt" % "2.1.0"
    object guice {
      lazy val core = "com.google.inject" % "guice" % Versions.guice
      lazy val multiBindings = "com.google.inject.extensions" % "guice-multibindings" % Versions.guice
    }

  }

  object test {
    lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalatest
    lazy val specs2 = "org.specs2" %% "specs2" % Versions.specs2
  }


  lazy val defaultTestingLibs = {
    import test._

    def inTest(module: ModuleID) = module % "test"

    Seq (
      inTest(scalaTest),
      inTest(specs2)
    )
  }

}
