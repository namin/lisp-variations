# meta-boot

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

- `lisp.scala` is an LMS-staged interpreter.

- `lispi.scala` is the corresponding unstaged interpreter.

- `lispb.scala` turns all the interpreter structures into object
  structures (primitives, environment, continuations), so we can later
  do reification/reflection.

- `lispc.scala` adds `FSUBR` and exposes the interpreter functions,
  adds `FEXPR` and implements a set! with history tracking.


## todo

- `lispd.scala` now adds compilation back. How do we achieve that? We
  have to compile wrt to the current semantics. We need a way to
  distinguish environment entries that are stable. We put compilation
  under user control, with lambda as a unit.
