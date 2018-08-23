package lispb

object ast {
  trait Value
  case object Undefined extends Value
  case class B(b: Boolean) extends Value                      // Boolean
  case class I(n: Int) extends Value                          // Int
  case class S(sym: String) extends Value                     // Symbol
  case object N extends Value                                 // Nil
  class P(var car: Value, var cdr: Value) extends Value       // Pair
  object P {
    def apply(a: Value, b: Value): P = new P(a, b)
    def unapply(v: Value): Option[(Value, Value)] = v match {
      case p:P => Some((p.car, p.cdr))
      case _ => None
    }
  }
  case class F(f: Value => Value) extends Value               // Procedures
  type Frame = scala.collection.mutable.Map[String,Value]
  type Env = List[Frame]
  type Cont = Value => Value

  def list(e: Value): List[Value] = e match {
    case N => Nil
    case P(first, rest) => first :: list(rest)
  }
}

import ast._
object eval {
  def base_eval(exp: Value, env: Env, cont: Cont): Value = {
    exp match {
      case I(_) | B(_) => cont(exp)
      case S(sym) => eval_var(exp, env, cont)
      case P(S("quote"), _) => eval_quote(exp, env, cont)
      case P(S("if"), _) => eval_if(exp, env, cont)
      case P(S("set!"), _) => eval_set_bang(exp, env, cont)
      case P(S("lambda"), _) => eval_lambda(exp, env, cont)
      case P(S("begin"), body) => eval_begin(body, env, cont)
      case P(S("define"), _) => eval_define(exp, env, cont)
      case P(fun, args) => eval_application(exp, env, cont)
    }
  }

  def eval_var(exp: Value, env: Env, cont: Cont): Value = exp match {
    case S(x) => get(env, x) match { case Some(v) => cont(v) }
  }

  def eval_quote(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(x, N)) => cont(x)
  }

  def eval_if(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(c, P(a, P(b, N)))) => base_eval(c, env, { cv => cv match {
      case B(false) => base_eval(b, env, cont)
      case B(true) => base_eval(a, env, cont)
    }})
  }

  def eval_set_bang(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(S(x), P(rhs, N))) => base_eval(rhs, env, { v =>
      set(env, x, v) match { case Some(v) => cont(v) }
    })
  }

  def eval_lambda(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(params, body)) => cont(F({args =>
      eval_begin(body, extend(env, params, args), {v => v})
    }))
  }

  def eval_begin(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(e, N) => base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, { _ => eval_begin(es, env, cont) })
  }

  def eval_define(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(r@S(name), body)) => {
      env.head(name) = Undefined
      eval_begin(body, env, {v =>
        env.head(name) = v
        cont(r)})
    }
  }

  def eval_application(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(fun, args) => base_eval(fun, env, { vf => vf match {
      case F(f) => evlist(args, env, { vas => cont(f(vas)) })
    }})
  }

  def evlist(exp: Value, env: Env, cont: Value => Value): Value = exp match {
    case N => cont(N)
    case P(first, rest) => base_eval(first, env, {v => evlist(rest, env, {vs => cont(P(v,vs))})})
  }

  def extend(env: Env, params: Value, args: Value): Env = {
    val frame: Frame = collection.mutable.Map((list(params).map(_.asInstanceOf[S].sym) zip list(args)) : _*)
    frame::env
  }

  def get(env: Env, x: String): Option[Value] = env match {
    case Nil => None
    case first::rest => first.get(x) match {
      case Some(v) => Some(v)
      case None => get(rest, x)
    }
  }

  def set(env: Env, x: String, v: Value): Option[Value] = env match {
    case Nil => None
    case first::rest => first.get(x) match {
      case Some(_) => {
        first(x) = v
        Some(v)
      }
      case None => get(rest, x)
    }
  }

  def init_env: Env = (scala.collection.mutable.Map[String,Value](
    ("<" ->   F({args => args match { case P(I(a), P(I(b), N)) => B(a<b) }})),
    ("*" ->   F({args => args match { case P(I(a), P(I(b), N)) => I(a*b) }})),
    ("-" ->   F({args => args match { case P(I(a), P(I(b), N)) => I(a-b) }})),
    ("eq?" -> F({args => args match { case P(a, P(b, N)) => B(a==b) }})))) :: (Nil: Env)
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
  var global_env = init_env
  def parse(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    e
  }
  def evl(e: Value) = { base_eval(e, global_env, { v => v } ) }
  def ev(s: String) = evl(parse(s))
  def clean() = {
    global_env = init_env
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

import repl._
import utils._
class lispb_Tests extends TestSuite {
  test("(factorial 6)") {
    ev("""(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1))))))""")
    assertResult(I(720))(ev("(factorial 6)"))
  }

  test("(odd 7)") {
    ev("""(begin
(define even (lambda (n) (if (eq? n 0) #t (odd (- n 1)))))
(define odd (lambda (n) (if (eq? n 0) #f (even (- n 1)))))
)""")
    assertResult(B(true))(ev("(odd 7)"))
  }
}
