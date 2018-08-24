# A reflective `lisp.scala`

## How to run

`sbt test`

You can also start `sbt` interactively (with no arguments), and then
run only selected tests: `testOnly lisp.*`. By adding a tilde, as in
`~testOnly lisp.*`, these tests will be run interactively every time
your project changes.

## Your task

In this assignment, you will transform `lisp.scala`, a rather
conventional Lisp interpreter written in Scala, into a reflective
Lisp with the classic `FSUBR` and `FEXPR` constructs.

The provided `lisp.scala` already makes all interpreter structures
valid object structures: for example both environment `env` and
continuation `cont` are object `Value`s.

Your task is now to move all the primitive functions and special forms
into the environment, to achieve reflection, both structural
(inspecting meta-structures) and procedural (changing special
behaviors).

## Step 1

Change `ignore` to `test` for `(factorial 6)`. Add the missing
primitives to the environment.

There are other missing primitives, and you may want to add them as
you go. Only the primitives used in the tests will be tested, so do
not feel you need to be complete.

## Step 2

Make all the special forms (such as `quote`, `if`, `set!`, ...) into
environment functions by adding a new value for special functions,
called `Fsubr`. Then change `eval-application` to handle this new
value. Package all the arguments into one (object) list of values.

## Step 3

Change `ignore` to `test` for `eval`. Add the missing primitive to the
environment.

## Step 4

Change the remaining `ignore` calls to `test` calls. Implement a third
variant of functions (besides `lambda` and `fsubr`) called `fexpr`
with a new value for functions called `Fexpr`. The `Fexpr` variant is
called with unevaluated arguments (unlike `lambda`), but does not
package the interpreter structures for environment `env` and
continuation `cont` (unlike `fsubr`).

## Step 5

Add your own test demonstrating some reflection or reification.
