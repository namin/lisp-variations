lazy val root = (project in file("."))
  .settings(
    name := "lisp-variations",
    scalaVersion := "3.3.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19"
  )
