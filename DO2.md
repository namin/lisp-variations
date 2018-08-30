# A `lisp0.scala` for easy compilation

## How to run

`sbt test`

You can also start `sbt` interactively (with no arguments), and then
run only selected tests: `testOnly lisp0.*`. By adding a tilde, as in
`~testOnly lisp0.*`, these tests will be run interactively every time
your project changes.

For now, the tests are commented out, because you need to complete a
bit of the task for the project to compile.

## Your task

In this assignment, you will transform `lisp0.scala` as defined in the
`trait Eval` from an interpreter to a compiler. You will do this by
using [Lightweight Modular Staging (LMS)](http://scala-lms.github.io).

The provided code already has all the LMS facilities you are likely to
need. Your role is to think about how to stage the interpreter
principally, driven by types.

For an expression that executes "now", at code generation time, use a
bare type, such as `Int`. For an expression that is delayed to the
generated code use a `Rep` type, such as `Rep[Int]`.

In order to represent mutation, you can use the LMS-provided `Var`
type. This will work nicely here, because we are doing whole-program
code generation.

To support recursive functions, use LMS's `doLambda` to transform from a function of `Rep` types to a `Rep` of a function: for example, from `Rep[Int] => Rep[Int]` to `Rep[Int => Int]`.

The starting point is an interpreter that is considerably less
flexible than what you saw in the first assignment. It has a
separation of `Term` and `Value`, and in fact, `Value` is only `Int`,
which facilitates generating C code without worrying about tags.

Use compilation error(s), including the initial errors when you
uncomment the later part of the file, by fixing the program driven by
types. What should a staged continuation look like? A function?
An environment?

The directory `out` contains the expected generated code, but some
variation is OK.
