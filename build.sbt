ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "lua2cpg"
  )

libraryDependencies ++= Seq(
  "io.joern" %% "x2cpg" % "2.0.99",
  "org.slf4j" % "slf4j-api" % "2.0.7",
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.20.0" % Optional,
  "org.apache.logging.log4j" % "log4j-core" % "2.20.0" % Optional,
  "org.luaj" % "luaj-jse" % "3.0.1",
  "com.typesafe.play" %% "play-json" % "2.10.3",
  "com.lihaoyi"   %% "ujson" % "3.1.3"
)