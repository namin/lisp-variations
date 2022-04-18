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

Running `./ex.sh` will produce folders and zip files for exercise `ex1`.
For exercise `ex2`, see the [LMS branch](https://github.com/namin/lisp-variations/tree/lms).

## done

- The [LMS branch](https://github.com/namin/lisp-variations/tree/lms) has some variations that use [Lightweight Modular Staging (LMS)](https://scala-lms.github.io/tutorials).

- [`lispi.scala`](lispi.scala) is the basic interpreter.

- [`lispb.scala`](lispb.scala) turns all the interpreter structures into object
  structures (primitives, environment, continuations), so we can later
  do reification/reflection.

- [`lispc.scala`](lispc.scala) adds `FSUBR` and exposes the interpreter functions,
  adds `FEXPR` and implements a `set!` with history tracking.

- [`lispd.scala`](lispd.scala) adds `call/cc` as a built-in or user-defined form.

## todo

- How much of Purple's infrastructure do we want to re-use? What is the intellectual contribution wrt to Purple?

- Thinking by-hand about the compilation FEXPRs. What should happen for `(foo (+ 1 2))`? or `(lambda (foo) (foo (+ 1 2)))`?

- Consider a Just-in-Time model of guarded optimizations.

- Consider open implementations. How can the system provide compilation / implementation directives?

- Consider restrictions to the semantics so that compilation is more reasonable. (How, without losing the spirit?)

- Contrast FEXPRs implemented as a user-level concept in
  [Black](https://github.com/namin/black/blob/fexpr/examples/fexpr.blk)
  /
  [Purple](https://github.com/namin/lms-black/blob/master/src/test/scala/lms/black/fexpr.scala)
  with the built-in FEXPRs here.
  Explore compilation and "bugs" (does compilation change the semantics? maybe not, because not optimizing compiler?)
  in FEXPRs in Purple.

- Try modifying `base-eval` so that it records the calls to it as they happen.
  Should notice that base operations are not affected, only explicit calls to `base-eval`.
  How could `base-eval` be truly modifiable? Would it cause an infinite recursion, unlike in the tower model?

- Try making `base-apply` more flexible, adding new custom calling conventions.

- Consider making environment functions like in Brown'84 to support custom behaviors during lookup?
  Or add a lookup function?

- Add compilation to `lispc.scala`. How do we achieve that? We
  have to compile wrt to the current semantics. We need a way to
  distinguish environment entries that are stable. We put compilation
  under user control, with functions as a unit. What do we do with
  `FEXPR`? Impose restrictions? Perhaps, follow the [Purple](https://github.com/namin/lms-black) model.

- Support macros in macros naturally? Do we need a macro calling convention? Do we need to define new calling convention at the user level?

- Support macroexpansion separate from evaluation at the user level.

- Implement quasiquotes in the reader, and then as a user-level macro.

- How could one implement vectors at the user level?

- Implement structs at the user level (using a macro), possibly following [Matt Might's blogpost](https://matt.might.net/articles/implementation-of-scheme-vector-struct-in-syntax-rules/).

- Implement a file loader.

- Support variadic arguments.

- Functions are opaque. Open them?
