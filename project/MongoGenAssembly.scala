import Libraries._

object MongoGenAssembly extends BaseModule {
  val moduleName = "mongo-gen-assembly"
  val location = "./mg-assembly"

  val settings = Seq ()

  def project = baseProject

  lazy val libraries = Seq (
    main.scopt
  )
}
