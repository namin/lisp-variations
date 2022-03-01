# Lisp Variations

This development served as the basis for some assignment in the [Course on Metaprogramming](https://github.com/namin/metaprogramming), University of Cambridge, UK Michaelmas Term 2018.

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

## done

- `lisps.scala` is an LMS-staged interpreter.

- `lisp0.scala` is an LMS-staged interpreter, dumbed down to be C friendly.

- `lisp0a.scala` adds nested functions to `lisp0.scala` by basic
  closure conversion, relying on LMS batteries to get the free
  variables of a function body.

- `lispi.scala` is the unstaged interpreter corresponding to `lisps.scala`.

- `lispb.scala` turns all the interpreter structures into object
  structures (primitives, environment, continuations), so we can later
  do reification/reflection.

- `lispc.scala` adds `FSUBR` and exposes the interpreter functions,
  adds `FEXPR` and implements a `set!` with history tracking.

- `lispd.scala` adds `call/cc` as a built-in or user-defined form.

## todo

- Add compilation back to `lispc.scala`. How do we achieve that? We
  have to compile wrt to the current semantics. We need a way to
  distinguish environment entries that are stable. We put compilation
  under user control, with functions as a unit. What do we do with
  `FEXPR`? Impose restrictions?

- Compile `lisps.scala` to C by program transformations. Start bottom
  up from `lisp0.scala`.
