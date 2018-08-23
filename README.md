# meta-boot

- `lisp.scala` is an LMS-staged interpreter.

- `lispi.scala` is the corresponding unstaged interpreter.

- `lispb.scala` turns all the interpreter structures into object
  structures, so we can later do reflection.

- `lispc.scala` adds `FSUBR` and exposes the interpreter functions, adds `FEXPR`.
