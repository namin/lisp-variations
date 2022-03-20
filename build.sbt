lazy val root = (project in file("."))
  .settings(
    name := "lisp-variations",
    scalaVersion := "3.1.1",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11"
  )
