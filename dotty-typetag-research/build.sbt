lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-example-project",
    description := "Example sbt project that compiles using Dotty",
    version := "0.1.0",
    scalacOptions ++= Seq("-Ykind-projector", "-language:implicitConversions"),
    scalaVersion := "0.22.0-RC1",
  )
