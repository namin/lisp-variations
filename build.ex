libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

console / initialCommands := "import lisp._; import repl._"

scalaSource in Test := baseDirectory.value

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5"
