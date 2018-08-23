# meta-boot

- `lisp.scala` is an LMS-staged interpreter.

- `lispi.scala` is the corresponding unstaged interpreter.

- `lispb.scala` turns all the interpreter structures into object
  structures (primitives, environment, continuations), so we can later
  do reification/reflection.

- `lispc.scala` adds `FSUBR` and exposes the interpreter functions,
  adds `FEXPR` and implements a set! with history tracking.
