import sbt._
import Libraries._
import Modules._

object MongoGenParser extends BaseModule {
  val moduleName = "mongo-gen-parser"
  val location = "./mg-parser"

  val settings = Seq ()

  def project = baseProject dependsOn (mgCommon)

  lazy val libraries = Seq ()
}
