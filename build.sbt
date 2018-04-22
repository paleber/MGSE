lazy val root = (project in file("."))
  .settings(
    name := "MSGE",
    organization := "de.htwg",
    scalaVersion := "2.12.4",
    version := "0.1.0-SNAPSHOT",
    fork := true,

    libraryDependencies += "org.scala-lang.modules" %% "scala-xml_2.11" % "1.0.5"
  )


