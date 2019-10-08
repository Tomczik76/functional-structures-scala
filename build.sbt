lazy val root = (project in file("."))
  .settings(
    name := "Hello",
    scalaVersion := "2.12.10",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "simulacrum" % "1.0.0",
      "co.fs2" %% "fs2-core" % "1.1.0-M2",
      "co.fs2" %% "fs2-io" % "1.1.0-M2",
      "org.scala-lang" % "scala-reflect" % "2.12.10"
    ),
    resourceDirectory in Compile := baseDirectory.value / "resources",
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
