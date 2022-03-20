# Lisp Variations

This development served as the basis for some assignment in the [Course on Metaprogramming](https://github.com/namin/metaprogramming), University of Cambridge, UK Michaelmas Term 2018.
It will now be extended to further experimentations.

## Motivation

### Quotes by Alan Kay

> The MOP example is to decide after the fact to change the execution
> strategy and tactics for how dynamic instances are represented while
> running -- e.g, can you decide that some objects should better use
> hash tables rather than arrays to represent them?”

> Similarly, the MOP example is *really interesting* if the new
> behaviors added via reflective access are automatically compiled to
> be part of the base languages machinery (rather than being an
> interpreted add-on).

## learn by doing

Running `./ex.sh` will produce folders and zip files for exercises `ex1` (recommended) and `ex2`.

## done

- [`lisps.scala`](lisps.scala) is a staged interpreter, using [Lightweight Modular Staging (LMS)](https://scala-lms.github.io/tutorials).

- [`lisp0.scala`](lisp0.scala) is an LMS-staged interpreter, dumbed down to be C friendly.

- [`lisp0a.scala`](lisp0a.scala) adds nested functions to `lisp0.scala` by basic
  closure conversion, relying on LMS batteries to get the free
  variables of a function body.

- [`lispi.scala`](lispi.scala) is the unstaged interpreter corresponding to `lisps.scala`.

- [`lispb.scala`](lispb.scala) turns all the interpreter structures into object
  structures (primitives, environment, continuations), so we can later
  do reification/reflection.

- [`lispc.scala`](lispc.scala) adds `FSUBR` and exposes the interpreter functions,
  adds `FEXPR` and implements a `set!` with history tracking.

- [`lispd.scala`](lispd.scala) adds `call/cc` as a built-in or user-defined form. Unrelatedly, adds an implementation of `define-macro` in terms of `fexpr`.

## todo

- Add compilation back to `lispc.scala`. How do we achieve that? We
  have to compile wrt to the current semantics. We need a way to
  distinguish environment entries that are stable. We put compilation
  under user control, with functions as a unit. What do we do with
  `FEXPR`? Impose restrictions? Perhaps, follow the [Purple](https://github.com/namin/lms-black) model.

- Compile `lisps.scala` to C by program transformations. Start bottom
  up from `lisp0.scala`.

- Support macros in macros correctly?

- Support macroexpansion separate from evaluation at the user level.

- Implement quasiquotes in the reader, and then as a user-level macro.

- How could one implement vectors at the user level?

- Implement structs at the user level (using a macro), possibly following [Matt Might's blogpost](https://matt.might.net/articles/implementation-of-scheme-vector-struct-in-syntax-rules/).

- Implement a file loader.

- Support variadic arguments.

- Functions are opaque. Open them?
