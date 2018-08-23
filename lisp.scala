package lisp

import lms._

object ast {
  trait Value
  case class I(n: Int) extends Value
  case class B(b: Boolean) extends Value
  case class S(sym: String) extends Value
  case object N extends Value
  case class P(car: Value, cdr: Value) extends Value
  case class Clo(fun: Value => Value) extends Value

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
trait Eval extends LMS_Base with scala.lms.common.EffectExp {
  implicit def valueTyp: Typ[Value]

  type Cont = Rep[Value] => Rep[Value]
  type Env = List[scala.collection.mutable.Map[String,Var[Value]]]

  def clo(f: Rep[Value => Value]): Rep[Value]
  def apply_fun(f: Rep[Value], args: List[Rep[Value]]): Rep[Value]

  def base_eval(exp: Value, env: Env, cont: Cont): Rep[Value] = {
    exp match {
      case I(_) | B(_) => cont(unit(exp))
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

  def eval_var(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case S(x) => get(env, x) match { case Some(v) => cont(v) }
  }
  def eval_quote(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(_, P(x, N)) => cont(unit(x))
  }
  def eval_if(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(_, P(c, P(a, P(b, N)))) => base_eval(c, env, { cv =>
      if (cv == unit(B(false):Value)) base_eval(b, env, cont) else base_eval(a, env, cont)
    })
  }
  def eval_set_bang(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(_, P(S(x), P(rhs, N))) => base_eval(rhs, env, { v =>
      set(env, x, v) match { case Some(v) => cont(v) }
    })
  }

  def eval_lambda(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(_, P(params, body)) => {
      val ps = list(params).map{x => x.asInstanceOf[S].sym}
      val f = {args: Rep[Value] =>
        eval_begin(body, extend(env, ps, formList(ps, args).map{v => var_new(v)}), { v => v })}
      val v = doLambda{f}
      cont(clo(v))
    }
  }
  def eval_begin(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(e, N) => base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, { _ => eval_begin(es, env, cont) })
  }
  def eval_define(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(_, P(r@S(name), body)) => {
      env.head(name) = var_new(unit(I(0)/*undefined*/:Value))
      eval_begin(body, env, {v =>
        val lhs: Var[Value] = env.head(name)
        var_assign(lhs, v)
      cont(unit(r:Value))})
    }
  }
  def eval_application(exp: Value, env: Env, cont: Cont): Rep[Value] = exp match {
    case P(fun, args) => evlist(args, env, { vas => fun match {
      case S(p) if prims.contains(p) => cont(apply_primitive(p, vas))
      case _ => base_eval(fun, env, { vf => cont(apply_fun(vf, vas)) })
    }})
  }

  def evlist(exp: Value, env: Env, k: List[Rep[Value]] => Rep[Value]): Rep[Value] = exp match {
    case N => k(Nil)
    case P(first, rest) => base_eval(first, env, {v => evlist(rest, env, {vs => k(v::vs)})})
  }
  def formList(ps: List[String], args: Rep[Value]): List[Rep[Value]] = ps match {
    case Nil => Nil
    case _::rest => car(args)::formList(rest, cdr(args))
  }

  def extend(env: Env, params: List[String], args: List[Var[Value]]): Env =
    collection.mutable.Map(((params zip args) : _*))::env

  def get(env: Env, x: String): Option[Rep[Value]] = env match {
    case Nil => None
    case first::rest => first.get(x) match {
      case Some(v) => Some(v)
      case None => get(rest, x)
    }
  }

  def set(env: Env, x: String, v: Rep[Value]): Option[Rep[Value]] = env match {
    case Nil => None
    case first::rest => first.get(x) match {
      case Some(_) => {
        var_assign(first(x), v)
        Some(v)
      }
      case None => set(rest, x, v)
    }
  }

  def apply_primitive(p: String, args: List[Rep[Value]]): Rep[Value]
  def car(v: Rep[Value]) = apply_primitive("car", List(v))
  def cdr(v: Rep[Value]) = apply_primitive("cdr", List(v))

  def init_env: Env = (scala.collection.mutable.Map[String,Var[Value]]()) :: (Nil: Env)
}

trait EvalExp extends Eval with LMS_Exp {
  override implicit def valueTyp: Typ[Value] = manifestTyp[Value]

  case class MakeClo(f: Rep[Value => Value]) extends Def[Value]
  case class ApplyFun(fun: Rep[Value], args: List[Rep[Value]]) extends Def[Value]
  case class ApplyPrimitive(p: String, args: List[Rep[Value]]) extends Def[Value]

  def effectful(p: String) = p=="newline" || p=="display"
  override def apply_primitive(p: String, args: List[Rep[Value]]): Rep[Value] = {
    if (!effectful(p)) {
      if (args.forall(_.isInstanceOf[Const[Value]])) {
        return Const(apply_primitive_on_values(p, args.map(_.asInstanceOf[Const[Value]].x)))
      }
      return ApplyPrimitive(p, args)
    }
    return reflectEffect(ApplyPrimitive(p, args))
  }

  def clo(f: Rep[Value => Value]): Rep[Value] = MakeClo(f)
  def apply_fun(fun: Rep[Value], args: List[Rep[Value]]): Rep[Value] = reflectEffect(ApplyFun(fun, args))
}

trait ScalaGenEval extends scala.lms.common.ScalaGenEffect {
  val IR: EvalExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ApplyPrimitive(p, args) => emitValDef(sym, s"""apply_primitive_on_values(\"$p\", ${args.map(quote)})""")
    case ApplyFun(fun, args) => emitValDef(sym, s"""${quote(fun)}.asInstanceOf[Clo].fun(valueOf(${args.map(quote)}))""")
    case MakeClo(f) => emitValDef(sym, src"""Clo($f)""")
    case _ => super.emitNode(sym, rhs)
  }
}

trait EvalSnippet extends EvalExp with scala.lms.common.CompileScala { q =>
  val codegen = new LMS_Gen with ScalaGenEval {
    val IR: q.type = q
    override def emitSource[A : Typ](args: List[Sym[_]], body: Block[A], className: String, stream: java.io.PrintWriter): List[(Sym[Any], Any)] = {
      stream.println("import lisp._")
      stream.println("import ast._")
      super.emitSource(args, body, className, stream)
    }
    override def remap[A](m: Typ[A]): String = {
      if (m.toString.endsWith("ast$Value")) "Value"
      else super.remap(m)
    }
  }
  def snippet(x: Rep[Value]): Rep[Value]
  lazy val f = compile(snippet)
  def precompile: Unit = f
  def eval(x: Value): Value = f(x)
  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
    source.toString
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

import parser._

object repl {
  def parse(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    e
  }
  def evl(e: Value): Value => Value = {
    val snippet = new EvalSnippet {
      override def snippet(x: Rep[Value]): Rep[Value] = {
        var lastFun: Rep[Value] = null
        base_eval(e, init_env, {v =>
          lastFun = v; v
        })
        apply_fun(lastFun, List(x))
      }
    }
    println(snippet.code)
    snippet.f
  }
  def ev(s: String) = evl(parse(s))
}

import repl._
import utils._
class lisp_Tests extends TestSuite {
  test("(factorial 6)") {
    val factorial = ev("""(begin
(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1))))))
factorial
)""")
    assertResult(I(720))(factorial(I(6)))
  }

  test("(odd 7)") {
    val odd = ev("""(begin
(define odd (lambda (n) n))
(define even (lambda (n) (if (eq? n 0) #t (odd (- n 1)))))
(set! odd (lambda (n) (if (eq? n 0) #f (even (- n 1)))))
odd
)""")
    assertResult(B(true))(odd(I(7)))
  }
}
