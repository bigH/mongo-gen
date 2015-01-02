import sbt._
import Libraries._
import Modules._

object MongoGenCasbahGenerator extends BaseModule {
  val moduleName = "mongo-gen-casbah-generator"
  val location = "./mg-casbah-generator"

  val settings = Seq ()

  def project = baseProject dependsOn (mgGeneratorApi)

  lazy val libraries = Seq ()
}
