package lispd

object ast {
  trait Value
  case object Undefined extends Value
  case class B(b: Boolean) extends Value                      // Boolean
  case class I(n: Int) extends Value                          // Int
  case class S(sym: String) extends Value                     // Symbol
  case object N extends Value                                 // Nil
  class P(var car: Value, var cdr: Value) extends Value       // Pair
  {
    override def toString = s"@${super.toString}($car, $cdr)"
  }
  object P {
    def apply(a: Value, b: Value): P = new P(a, b)
    def unapply(v: Value): Option[(Value, Value)] = v match {
      case p:P => Some((p.car, p.cdr))
      case _ => None
    }
  }
  case class F(f: Value  => C => Value) extends Value         // Functions
  case class Fsubr(f: Value => Value) extends Value           // FSUBR -- for exposed interpreter functions
  case class Fexpr(f: Value => Value) extends Value           // FEXPR -- unevaluated arguments

  // Env is a list of frames (each a list of key/value pairs)
  // We use object structures for easy reification/reflection.
  type Env = P
  // Similarly, continuations are values too...
  // ... but we keep them distinguished from functions
  // ... because the calling convention differs
  case class C(f: Value => Value) extends Value               // Continuations
  type Cont = C

  def list(e: Value): List[Value] = e match {
    case N => Nil
    case P(first, rest) => first :: list(rest)
  }
  def valueOf(es: List[Value]): Value = es match {
    case Nil => N
    case first::rest => P(first, valueOf(rest))
  }
  def fsubrOf(f: (Value, Env, Cont) => Value) = Fsubr{ v => v match {
    case P(exp, P(env:P, P(cont:C, N))) => f(exp, env, cont)
  }}
}

import ast._
object eval {
  def base_eval(exp: Value, env: Env, cont: Cont): Value = debug(s"eval ${pp.show(exp)}", env, cont) { (cont) =>
    exp match {
      case I(_) | B(_) => cont.f(exp)
      case S(sym) => eval_var(exp, env, cont)
      case P(fun, args) => base_apply(exp, env, cont)
    }
  }

  def base_apply(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(fun, args) => base_eval(fun, env, C{ vf => vf match {
      case F(f) => evlist(args, env, C{ vas => f(vas)(cont) })
      case C(f) => evlist(args, env, C{ case P(v, N) => f(v) })
      case Fsubr(f) => f(P(exp, P(env, P(cont, N))))
      case Fexpr(f) => cont.f(f(args))
    }})
  }

  def eval_callcc(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(f, N)) =>
      base_apply(P(f, P(P(S("quote"), P(cont, N)), N)), env, cont)
  }

  def eval_var(exp: Value, env: Env, cont: Cont): Value = exp match {
    case S(x) => cont.f(get(env, x))
  }

  def eval_quote(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(x, N)) => cont.f(x)
  }

  def eval_if(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(c, P(a, P(b, N)))) => base_eval(c, env, C{ cv => cv match {
      case B(false) => base_eval(b, env, cont)
      case B(true) => base_eval(a, env, cont)
    }})
  }

  def eval_set_bang(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(S(x), P(rhs, N))) => base_eval(rhs, env, C{ v =>
      cont.f(set(env, x, v))
    })
  }

  def eval_fun(c: (Value => Value) => Value)(exp: Value, env: Env, cont: Cont): Value =  exp match {
    case P(_, P(params, body)) => cont.f(c({args =>
      eval_begin(body, extend(env, params, args), C{v => v})
    }))
  }
  def eval_lambda(exp: Value, env: Env, cont: Cont): Value =  exp match {
    case P(_, P(params, body)) => cont.f(F({args => c =>
      eval_begin(body, extend(env, params, args), c)
    }))
  }
  def eval_fsubr = eval_fun(Fsubr) _
  def eval_fexpr = eval_fun(Fexpr) _

  def eval_begin_exp(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, body) =>  eval_begin(body, env, cont)
  }
  def eval_begin(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(e, N) => base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, C{ _ => eval_begin(es, env, cont) })
  }

  def eval_define(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(r@S(name), body)) => {
      val p = P(r,Undefined)
      env.car = P(p, env.car)
      eval_begin(body, env, C{v =>
        p.cdr = v
        cont.f(r)})
    }
  }

  def evlist(exp: Value, env: Env, cont: Cont): Value = exp match {
    case N => cont.f(N)
    case P(first, rest) => base_eval(first, env, C{v => evlist(rest, env, C{vs => cont.f(P(v,vs))})})
  }

  def extend(env: Env, params: Value, args: Value): Env = {
    val frame = valueOf((list(params) zip  list(args)).map{t => P(t._1, t._2)})
    P(frame, env)
  }

  def findFrame(frame: Value, x: String): Option[P] = frame match {
    case N => None
    case P(P(S(y),_), _) if (x==y) => Some(frame.asInstanceOf[P].car.asInstanceOf[P])
    case P(_, rest) => findFrame(rest, x)
  }
  def find(env: Env, x: String): P = env match {
    case P(first,rest) => findFrame(first, x) match {
      case Some(p) => p
      case None => rest match {
        case next:Env => find(next, x)
        case _ => sys.error(s"unbound variable $x")
      }
    }
  }
  def get(env: Env, x: String): Value = find(env, x).cdr
  def set(env: Env, x: String, v: Value): Value = {
    val p = find(env, x)
    p.cdr = v
    v
  }

  def FC(f: Value => Value) = F{v => c => c.f(f(v))}

  def make_init_env(): Env = {
  lazy val init_env: Env = P(valueOf(List(
    P(S("<"),   FC({args => args match { case P(I(a), P(I(b), N)) => B(a<b) }})),
    P(S("*"),   FC({args => args match { case P(I(a), P(I(b), N)) => I(a*b) }})),
    P(S("-"),   FC({args => args match { case P(I(a), P(I(b), N)) => I(a-b) }})),
    P(S("eq?"), FC({args => args match { case P(a, P(b, N)) => B(a==b) }})),
    P(S("cons"), FC({args => args match { case P(a, P(b, N)) => P(a, b) }})),
    P(S("car"),  FC({args => args match { case P(P(a, d), N) => a }})),
    P(S("cdr"),  FC({args => args match { case P(P(a, d), N) => d }})),
    P(S("list"), FC({args => args})),
    P(S("quote"), fsubrOf(eval_quote)),
    P(S("if"), fsubrOf(eval_if)),
    P(S("set!"), fsubrOf(eval_set_bang)),
    P(S("lambda"), fsubrOf(eval_lambda)),
    P(S("fsubr"), fsubrOf(eval_fsubr)),
    P(S("fexpr"), fsubrOf(eval_fexpr)),
    P(S("begin"), fsubrOf(eval_begin_exp)),
    P(S("define"), fsubrOf(eval_define)),
    P(S("call/cc"), fsubrOf(eval_callcc)),
    P(S("base-eval"), F{args => c => args match { case P(exp, P(e, P(k, N))) =>
      val env = e match {
        case N => init_env
        case e:P => e
      }
      val cont = k match {
        case k:F => C{v => k.f(P(v, N))(c)}
        case k:C => k
      }
      base_eval(exp, env, cont)
    }}),
    P(S("eval"), F({args => c => args match { case P(exp, N) => base_eval(exp, init_env, c) }}))
  )), N)
    init_env
  }
}

import scala.util.parsing.combinator._
object parser extends JavaTokenParsers with PackratParsers {
    def exp: Parser[Value] =
      "#f" ^^ { case _ => B(false) } |
      "#t" ^^ { case _ => B(true) } |
      wholeNumber ^^ { case s => I(s.toInt) } |
      """[^\s\(\)'"]+""".r ^^ { case s => S(s) } |
      "'" ~> exp ^^ { case s => P(S("quote"), P(s, N)) } |
      "()" ^^ { case _ => N } |
      "(" ~> exps <~ ")" ^^ { case vs => vs }

  def exps: Parser[Value] =
      exp ~ exps ^^ { case v~vs => P(v, vs) } |
      exp ^^ { case v => P(v, N) }
}

import eval._
import parser._
object repl {
  var global_env = make_init_env()
  def parse(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    e
  }
  def evl(e: Value) = { base_eval(e, global_env, C{ v => v } ) }
  def ev(s: String) = evl(parse(s))
  def clean() = {
    global_env = make_init_env()
  }
}

object pp {
  def addParen(p: (Boolean, String)) = {
    val (need_paren, s) = p
    if (need_paren) "("+s+")" else s
  }
  def pp(v: Value): (Boolean, String) = v match {
    case B(b) => (false, if (b) "#t" else "#f")
    case I(n) => (false, n.toString)
    case S(s) => (false, s)
    case N => (true, "")
    case P(a, N) => (true, addParen(pp(a)))
    case P(a, d) =>
      val s1 = addParen(pp(a))
      val (need_paren2, s2) = pp(d)
      if (need_paren2) (true, s1+" "+s2)
      else (true, s1+" . "+s2)
    case _ => (false, v.toString)
  }
  def show(v: Value) = addParen(pp(v))
  def display(v: Value) = print(show(v))
  def newline() = println("")
}

import ast._
object debug {
  val enable = true
  var depth: Int = 0
  val indentTab = " "

  def apply[T](msg: String, env: Env, cont: Cont)(body: Cont => T) = trace[T](msg, env, cont)(body)

  def trace[T](msg: String, env: Env, cont: Cont)(body: Cont => T) = {
    indentedDebug(s"==> ${pad(msg)}?")
    //indentedDebug(env.format)
    depth += 1
    val newCont = C{ v =>
      depth -= 1
      indentedDebug(s"<== ${pad(msg)} = ${pad(v.toString)}")
      cont.f(v)
    }
    body(newCont)
  }

  def padding = scala.collection.StringOps(indentTab) * depth

  def pad(s: String, padFirst: Boolean = false) =
    s.split("\n").mkString(if (padFirst) padding else "", "\n" + padding, "")

  def indentedDebug(msg: String) =
    if(enable) println(pad(msg, padFirst = true))

  implicit class EnvDeco(val env: Env) extends AnyVal {
    def format: String =
      "---------env-------\n" ++
      list(env).map(formatFrame).mkString("\n") ++
      "\n---------env-------\n"

    def formatFrame(frame: Value): String = list(frame).map {
      case P(S(name), body) => name + " -> " + body
    }.mkString("\n")
  }
}

import repl._
import pp._
import utils._
class lisp_Tests extends TestSuite {  before { clean() }
  test("(factorial 6)") {
    ev("""(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1))))))""")
    assertResult(I(720))(ev("(factorial 6)"))
  }

  test("eq?") {
    assertResult(B(true))(ev("(eq? 1 1)"))
    assertResult(B(false))(ev("(eq? 1 2)"))
    assertResult(B(false))(ev("(eq? (list 1) (list 1))"))
  }

  test("(odd 7)") {
    ev("""(begin
(define even (lambda (n) (if (eq? n 0) #t (odd (- n 1)))))
(define odd (lambda (n) (if (eq? n 0) #f (even (- n 1)))))
)""")
    assertResult(B(true))(ev("(odd 7)"))
  }

  test("eval") {
    ev("(define x 1)")
    assertResult(I(1))(ev("(eval 'x)"))
    assertResult(I(2))(ev("(* (eval 'x) 2)"))
  }

  test("call/cc") {
    assertResult(I(6))(ev("(* 2 (call/cc (lambda (k) 3)))"))
    assertResult(I(6))(ev("(* 2 (call/cc (lambda (k) (k 3))))"))
    assertResult(I(6))(ev("(* 2 (call/cc (lambda (k) (k (k 3)))))"))
    assertResult(I(6))(ev("(* 2 (call/cc (lambda (k) (* 5 (k 3)))))"))
  }

  test("my-call/cc") {
    ev("(define my-call/cc (fsubr (exp env cont) (base-eval (car (cdr exp)) env (lambda (f) (cont (f cont))))))")
    assertResult(I(6))(ev("(* 2 (my-call/cc (lambda (k) 3)))"))
    assertResult(I(6))(ev("(* 2 (my-call/cc (lambda (k) (k 3))))"))
    assertResult(I(6))(ev("(* 2 (my-call/cc (lambda (k) (k (k 3)))))"))
    assertResult(I(6))(ev("(* 2 (my-call/cc (lambda (k) (* 5 (k 3)))))"))
    assertResult(I(6))(ev("(* 2 ((lambda (x) (my-call/cc (lambda (k) (* 5 (k x))))) 3))"))
  }

  test("fexpr if") {
    ev("(define my-if (fexpr (c a b) (if (eval c) (eval a) (eval b))))")
    assertResult(I(1))(ev("(my-if #t 1 bad)"))
  }

  test("list") {
    // NOTE: we use `show` to compare pairs,
    // to by-pass referential equality.

    assertResult(ev("'()"))(ev("(list)"))
    assertResult(N)(ev("(list)"))

    assertResult(show(P(I(10), N)))(show(ev("(list 10)")))
    assertResult(show(P(I(10), N)))(show(ev("(cons '10 '())")))

    ev("(define history '())")
    assertResult(N)(ev("history"))
    assertResult(show(P(I(4), N)))(show(ev("(cons 4 history)")))
   }

  test("fexpr history") {
    ev("(define history '())")
    ev("""(define save! (fexpr (lhs rhs)
   ((lambda (old-val)
     (eval (list 'set! lhs rhs))
     (set! history (cons (list
        lhs
        old-val (eval lhs)) history)))
   (eval lhs))))""")
    ev("(define test 1)")
    ev("(save! test (* test 2))")
    assertResult(I(2))(ev("test"))
    assertResult("((test 1 2))")(show(ev("history")))
    ev("(save! test (* test 2))")
    assertResult(I(4))(ev("test"))
    assertResult("((test 2 4) (test 1 2))")(show(ev("history")))
  }

   test("fsubr") {
     ev("(define my-exp (fsubr (exp env cont) exp))")
     assertResult("(my-exp x)")(show(ev("(my-exp x)")))
     ev("(define jump (fsubr (exp env cont) (eval (car (cdr exp)))))")
     assertResult(I(2))(ev("(- 1 (jump 2))"))
     ev("(define fall (fsubr (exp env cont) 1))")
     assertResult(I(1))(ev("(* 2 (fall))"))
   }

  test("fsubr history") {
    ev("(define old-set! set!)")
    ev("(define history '())")
    ev("""(define save! (fexpr (lhs rhs)
   ((lambda (old-val)
     (eval (list 'old-set! lhs rhs))
     (old-set! history (cons (list
        lhs
        old-val (eval lhs)) history)))
   (eval lhs))))""")
    ev("""(set! set! (fsubr (exp env cont)
      (eval (list 'save! (car (cdr exp)) (car (cdr (cdr exp)))))
      (cont (car (cdr exp)))))""")
    ev("(define test 1)")
    ev("(set! test (* test 2))")
    assertResult(I(2))(ev("test"))
    assertResult("((test 1 2))")(show(ev("history")))
    ev("(set! test (* test 2))")
    assertResult(I(4))(ev("test"))
    assertResult("((test 2 4) (test 1 2))")(show(ev("history")))
  }

  test("trace") {
    ev("""
(define assq
  (lambda (k m)
    (if (eq? m '())
        #f
        (if (eq? (car (car m)) k)
            (car m)
            (assq k (cdr m))))))
""")
    ev("""(begin
(define t '())
(define traced-fns '())
(define trace (fexpr (fn)
(set! traced-fns (cons (cons fn (eval fn)) traced-fns))
((lambda (old-fn)
 ((lambda (tf)
   ((fsubr (_ env cont)
   (base-eval (list 'set! fn 'tf) env cont))))
  (lambda (x) (begin
    (set! t (cons (list fn x) t))
    ((lambda (v) (begin
     (set! t (cons (list fn x '=> v) t))
     v))
     (old-fn x))
  ))))
  (eval fn))))
(define untrace (fexpr (fn)
((lambda (old-fn)
((fsubr (_ env cont)
  (base-eval (list 'set! fn 'old-fn) env cont))))
(cdr (assq fn traced-fns)))
))
)""")
    ev("""(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1))))))""")
    ev("(trace factorial)")
    ev("(factorial 3)")
    assertResult("((factorial 3 => 6) (factorial 2 => 2) (factorial 1 => 1) (factorial 1) (factorial 2) (factorial 3))")(show(ev("t")))
    ev("(set! t '())")
    ev("(untrace factorial)")
    ev("(factorial 3)")
    assertResult("()")(show(ev("t")))
  }

  test("define-macro") {
    ev("""(begin
(define expand (lambda (binding body)
(list 'define (car binding)
(list 'fexpr (cdr binding) (list 'eval body)))
))
(define define-macro (fexpr (binding body)
  (eval (expand binding body))
))
)
""")
    assertResult(
      "(define quote-it (fexpr (x) (eval (list (quote quote) x))))")(
      show(ev("(expand '(quote-it x) '(list 'quote x))")))
    assertResult("y")(show(ev("""(begin
(define-macro (quote-it x) (list 'quote x))
(quote-it y)
)
""")))
    assertResult("(y 1)")(show(ev("""(begin
(define-macro (var-val x) (list 'list (list 'quote x) x))
(define y 1)
(var-val y)
)""")))

    ev("""(begin
(define-macro (foo x) x)
(define-macro (outer-foo x) (list 'foo x))
(define-macro (outer-foo2 x) (foo x))
)""")
    assertResult(I(1))(ev("(outer-foo 1)"))
    //assertResult(I(1))(ev("(outer-foo2 1)")) //TODO?
  }

}
