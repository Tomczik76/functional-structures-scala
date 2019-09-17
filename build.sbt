lazy val root = (project in file("."))
  .settings(
    name := "Hello",
    scalaVersion := "2.13.0",
    libraryDependencies ++= Seq(
      "com.github.mpilquist" %% "simulacrum" % "0.19.0",
      "co.fs2" %% "fs2-core" % "1.1.0-M2",
      "co.fs2" %% "fs2-io" % "1.1.0-M2"
    ),
    scalacOptions += "-Ymacro-annotations",
    resourceDirectory in Compile := baseDirectory.value / "resources"

  )
