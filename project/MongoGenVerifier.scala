import sbt._
import Libraries._
import Modules._

object MongoGenVerifier extends BaseModule {
  val moduleName = "mongo-gen-verifier"
  val location = "./mg-verifier"

  val settings = Seq ()

  def project = baseProject dependsOn (mgCommon)

  lazy val libraries = Seq (
    main.guice.core,
    main.guice.multiBindings
  )
}
