import sbt._
import Keys._
import net.virtualvoid.sbt.graph.{Plugin => GraphPlugin}
import de.johoop.cpd4sbt.CopyPasteDetector
import de.johoop.jacoco4sbt._
import JacocoPlugin._

object MyDefaults {
  lazy val settings =
    Defaults.defaultSettings ++
    CopyPasteDetector.cpdSettings ++
    jacoco.settings ++
    GraphPlugin.graphSettings ++ Seq (
      organization := "com.bigh",
      version      := Versions.mine,
      scalaVersion := Versions.scala
    ) ++ Seq (
      scalacOptions += "-feature",
      scalacOptions += "-unchecked",
      scalacOptions += "-deprecation"
    ) ++ Seq (
      resolvers += Resolver.sbtPluginRepo("releases")
    )

  lazy val testingLibs =
    Libraries.defaultTestingLibs
}
