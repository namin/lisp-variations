package lispi

object ast {
  trait Value
  case class I(n: Int) extends Value
  case class B(b: Boolean) extends Value
  case class S(sym: String) extends Value
  case object N extends Value
  case class P(car: Value, cdr: Value) extends Value
  case class Clo(params: Value, body: Value, env: Env) extends Value
  type Env = List[scala.collection.mutable.Map[String,Value]]

  def apply_primitive_on_values(p: String, args: List[Value]): Value = (p, args) match {
    case ("null?", List(a)) => B(a==N)
    case ("number?", List(a)) => B(a match {
      case I(_) => true
      case _ => false
    })
    case ("pair?", List(a)) => B(a match {
      case P(_, _) => true
      case _ => false
    })
    case ("symbol?", List(a)) => B(a match {
      case P(_, _) => true
      case _ => false
    })
    case ("<", List(I(a), I(b))) => B(a < b)
    case ("+", List(I(a), I(b))) => I(a+b)
    case ("-", List(I(a), I(b))) => I(a-b)
    case ("*", List(I(a), I(b))) => I(a*b)
    case ("car", List(v)) => car(v)
    case ("cdr", List(v)) => cdr(v)
    case ("cons", List(a, d)) => cons(a, d)
    case ("eq?", List(a, b)) => B(a==b)
    case ("display", List(a)) => display(a); I(0)
    case ("newline", Nil) => newline(); I(0)
    case _ => throw IllegalArgumentException("unsupported primitive call")
  }

  def cons(car: Value, cdr: Value) = P(car, cdr)
  def car(v: Value) = v match {
    case P(a, d) => a
  }
  def cdr(v: Value) = v match {
    case P(a, d) => d
  }

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

  def prims = Set(
    "null?",
    "number?",
    "pair?",
    "symbol?",
    "<",
    "+",
    "-",
    "*",
    "car",
    "cdr",
    "cons",
    "eq?",
    "display",
    "newline")

  def list(e: Value): List[Value] = e match {
    case N => Nil
    case P(first, rest) => first :: list(rest)
  }
  def valueOf(es: List[Value]): Value = es match {
    case Nil => N
    case first::rest => P(first, valueOf(rest))
  }
}

import ast._
object eval {
  type Cont = Value => Value

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
    case S(x) => cont(get(env, x).get)
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
      cont(set(env, x, v).get)
    })
  }

  def eval_lambda(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(params, body)) => cont(Clo(params, body, env))
  }
  def eval_begin(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(e, N) => base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, { _ => eval_begin(es, env, cont) })
  }
  def eval_define(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(r@S(name), body)) => {
      env.head(name) = I(0)
      eval_begin(body, env, {v =>
        env.head(name) = v
        cont(r)})
    }
  }
  def eval_application(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(fun, args) => evlist(args, env, { vas => fun match {
      case S(p) if prims.contains(p) => cont(apply_primitive_on_values(p, vas))
      case _ => base_eval(fun, env, { vf =>
        val Clo(params, body, clo_env) = vf
        val ps = list(params).map{x => x.asInstanceOf[S].sym}
        eval_begin(body, extend(clo_env, ps, vas), cont)
      })
    }})
  }

  def evlist(exp: Value, env: Env, k: List[Value] => Value): Value = exp match {
    case N => k(Nil)
    case P(first, rest) => base_eval(first, env, {v => evlist(rest, env, {vs => k(v::vs)})})
  }

  def extend(env: Env, params: List[String], args: List[Value]): Env =
    collection.mutable.Map((params zip args) : _*)::env

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
      case None => set(rest, x, v)
    }
  }

  def init_env: Env = (scala.collection.mutable.Map[String,Value]()) :: (Nil: Env)
}

import scala.util.parsing.combinator._
object parser extends JavaTokenParsers {
  def exp: Parser[Value] =
    "#f" ^^ { case _ => B(false) }
  | "#t" ^^ { case _ => B(true) }
  | wholeNumber ^^ { case s => I(s.toInt) }
  | """[^\s\(\)'"]+""".r ^^ { case s => S(s) }
  | "'" ~> exp ^^ { case s => P(S("quote"), P(s, N)) }
  | "()" ^^ { case _ => N }
  | "(" ~> exps <~ ")" ^^ { case vs => vs }

  def exps: Parser[Value] =
    exp ~ exps ^^ { case v~vs => P(v, vs) }
  | exp ^^ { case v => P(v, N) }
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

import repl._
import utils._
class lispi_Tests extends TestSuite {  before { clean() }
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
