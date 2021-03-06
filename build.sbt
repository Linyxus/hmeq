val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "hmeq",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"
  )
