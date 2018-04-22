lazy val root = (project in file("."))
  .settings(
    name := "MSGE",
    organization := "de.htwg",
    scalaVersion := "2.12.4",
    version := "0.1.0-SNAPSHOT",

    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.0"
  )


