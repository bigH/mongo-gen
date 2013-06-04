// runs copy-paste detector
addSbtPlugin("de.johoop" % "cpd4sbt" % "1.1.2")

// coverage
addSbtPlugin("de.johoop" % "jacoco4sbt" % "2.0.0")

// supports `assembly` task to build fat-jars
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.7")

// supports `dependency-tree` task to inspect dependencies
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.1")

// supports `eclipse` task to build eclipse project files
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0")

// supports `gen-idea` task to build .idea files
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0")

