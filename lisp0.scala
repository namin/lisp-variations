package lisp0

import lms._

object ast {
  trait Term
  case class I(n: Int) extends Term
  case class S(sym: String) extends Term
  case object N extends Term
  case class P(car: Term, cdr: Term) extends Term

  def apply_primitive_on_values(p: String, args: List[Int]): Int = (p, args) match {
    case ("<", List(a, b)) => if (a < b) 1 else 0
    case ("+", List(a, b)) => a+b
    case ("-", List(a, b)) => a-b
    case ("*", List(a, b)) => a*b
    case ("=", List(a, b)) => if (a==b) 1 else 0
    case ("display", List(a)) => println(a); 0
    case ("newline", Nil) => println(); 0
  }

  def prims = Set(
    "<",
    "+",
    "-",
    "*",
    "=",
    "display",
    "newline")
}

import ast._
trait Eval extends LMS_Base {
  type Cont = Rep[Int] => Rep[Int]
  type Env = List[scala.collection.mutable.Map[String,Var[Int]]]

  type Fun = Rep[Int => Int]
  var funs: scala.collection.mutable.Map[String,Fun] = scala.collection.mutable.Map()
  def apply_fun(n: String, args: List[Rep[Int]]): Rep[Int]

  def base_eval(exp: Term, env: Env, cont: Cont): Rep[Int] = {
    exp match {
      case I(a) => cont(unit(a))
      case S(sym) => eval_var(exp, env, cont)
      case P(S("if"), _) => eval_if(exp, env, cont)
      case P(S("set!"), _) => eval_set_bang(exp, env, cont)
      case P(S("begin"), body) => eval_begin(body, env, cont)
      case P(S("define"), _) => eval_define(exp, env, cont)
      case P(fun, args) => eval_application(exp, env, cont)
    }
  }

  def eval_var(exp: Term, env: Env, cont: Cont): Rep[Int] = exp match {
    case S(x) => get(env, x) match { case Some(v) => cont(v) }
  }
  def eval_if(exp: Term, env: Env, cont: Cont): Rep[Int] = exp match {
    case P(_, P(c, P(a, P(b, N)))) => base_eval(c, env, { cv =>
      if (cv == unit(0)) base_eval(b, env, cont) else base_eval(a, env, cont)
    })
  }
  def eval_set_bang(exp: Term, env: Env, cont: Cont): Rep[Int] = exp match {
    case P(_, P(S(x), P(rhs, N))) => base_eval(rhs, env, { v =>
      set(env, x, v) match { case Some(v) => cont(v) }
    })
  }
  def eval_begin(exp: Term, env: Env, cont: Cont): Rep[Int] = exp match {
    case P(e, N) => base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, { _ => eval_begin(es, env, cont) })
  }
  def eval_define(exp: Term, env: Env, cont: Cont): Rep[Int] = exp match {
    case P(_, P(r@S(name), body)) => {
      env.head(name) = var_new(unit(0))
      eval_begin(body, env, {v =>
        val lhs: Var[Int] = env.head(name)
        var_assign(lhs, v)
      cont(unit(0))})
    }
    case P(_, P(P(r@S(name), params), body)) => {
      val ps = list(params).map{x => x.asInstanceOf[S].sym}
      val f = {args: List[Rep[Int]] =>
        eval_begin(body, extend(env, ps, args.map{v => var_new(v)}), { v => v })}
      val v = ps.length match {
          case 1 => doLambda{x: Rep[Int] => f(List(x))}
      }
      funs(name) = v
      cont(unit(0))
    }
  }
  def eval_application(exp: Term, env: Env, cont: Cont): Rep[Int] = exp match {
    case P(fun, args) => evlist(args, env, { vas => fun match {
      case S(p) if prims.contains(p) => cont(apply_primitive(p, vas))
      case S(n) => cont(apply_fun(n, vas))
    }})
  }

  def evlist(exp: Term, env: Env, k: List[Rep[Int]] => Rep[Int]): Rep[Int] = exp match {
    case N => k(Nil)
    case P(first, rest) => base_eval(first, env, {v => evlist(rest, env, {vs => k(v::vs)})})
  }

  def list(e: Term): List[Term] = e match {
    case N => Nil
    case P(first, rest) => first :: list(rest)
  }

  def extend(env: Env, params: List[String], args: List[Var[Int]]): Env =
    collection.mutable.Map(((params zip args) : _*))::env

  def get(env: Env, x: String): Option[Rep[Int]] = env match {
    case Nil => None
    case first::rest => first.get(x) match {
      case Some(v) => Some(v)
      case None => get(rest, x)
    }
  }

  def set(env: Env, x: String, v: Rep[Int]): Option[Rep[Int]] = env match {
    case Nil => None
    case first::rest => first.get(x) match {
      case Some(_) => {
        var_assign(first(x), v)
        Some(v)
      }
      case None => set(rest, x, v)
    }
  }

  def apply_primitive(p: String, args: List[Rep[Int]]): Rep[Int] =  (p, args) match {
    case ("<", List(a, b)) => if (a < b) 1 else 0
    case ("+", List(a, b)) => a+b
    case ("-", List(a, b)) => a-b
    case ("*", List(a, b)) => a*b
    case ("=", List(a, b)) => if (a==b) 1 else 0
    case ("display", List(a)) => println(a); 0
    case ("newline", Nil) => println(""); 0
  }

  def init_env: Env = (scala.collection.mutable.Map[String,Var[Int]]()) :: (Nil: Env)
}

trait EvalExp extends Eval with LMS_Exp {
  case class ApplyFun(n: String, args: List[Rep[Int]]) extends Def[Int]

  def apply_fun(n: String, args: List[Rep[Int]]): Rep[Int] = reflectEffect(ApplyFun(n, args))

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ApplyFun(n, vs) if funs.contains(n) => syms(funs(n)) ::: syms(vs)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ApplyFun(n, vs) if funs.contains(n) => boundSyms(funs(n)) ::: boundSyms(vs)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ApplyFun(n, vs) if funs.contains(n) => symsFreq(funs(n)) ::: symsFreq(vs)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenEval extends scala.lms.common.ScalaGenEffect {
  val IR: EvalExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ApplyFun(n, args) => emitValDef(sym, s"""${quote(funs(n))}(${args.map(quote).mkString(",")})""")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenEval extends scala.lms.common.CGenEffect {
  val IR: EvalExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ApplyFun(n, args) => emitValDef(sym, s"""$n(${args.map(quote).mkString(",")})""")
    case e@Lambda(fun, x, y) => // done at top level
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Typ[A]): String = {
    val tpe = super.remap(m)
    if (tpe.startsWith("int") || tpe.startsWith("bool")) "int"
    else tpe
  }
  override def isPrimitiveType(tpe: String): Boolean = {
    tpe=="int" || super.isPrimitiveType(tpe)
  }

  override def emitSource[A:Typ](args: List[Sym[_]], body: Block[A], functionName: String, out: java.io.PrintWriter) = {
    val sA = remap(typ[A])
    withStream(out) {
      stream.println(sA+" "+functionName+"("+args.map(a => remapWithRef(a.tp)+" "+quote(a)).mkString(", ")+") {")

      emitBlock(body)

      val y = getBlockResult(body)
      if (remap(y.tp) != "void")
        stream.println("return " + quote(y) + ";")

      stream.println("}")
    }
    Nil
  }
}

trait EvalProg extends EvalExp {
  def snippet(x: Rep[Int]): Rep[Int]
}

trait EvalCSnippet extends EvalProg { q =>
  val codegen = new LMS_GenC with CGenEval {
    val IR: q.type = q
  }
  def emitAll(stream: java.io.PrintWriter): Unit = {
    val s = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(s))
    for ((k,_) <- funs) {
      stream.println(s"int $k(int arg);")
    }
    for ((k,Def(Lambda(fun:(Rep[Int] => Rep[Int]), _, _))) <- funs) {
      codegen.emitSource[Int,Int](fun, k, stream)
    }
    stream.println(s)
  }
  lazy val code: String = {
    val source = new java.io.StringWriter()
    emitAll(new java.io.PrintWriter(source))
    source.toString
  }
}

trait EvalSnippet extends EvalProg with scala.lms.common.CompileScala { q =>
  val codegen = new LMS_Gen with ScalaGenEval {
    val IR: q.type = q
  }
  lazy val f = compile(snippet)
  def precompile: Unit = f
  def eval(x: Int): Int = f(x)
  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
    source.toString
  }
}

import scala.util.parsing.combinator._
object parser extends JavaTokenParsers with PackratParsers {
    def exp: Parser[Term] =
      wholeNumber ^^ { case s => I(s.toInt) } |
      """[^\s\(\)'"]+""".r ^^ { case s => S(s) } |
      "()" ^^ { case _ => N } |
      "(" ~> exps <~ ")" ^^ { case vs => vs }

  def exps: Parser[Term] =
      exp ~ exps ^^ { case v~vs => P(v, vs) } |
      exp ^^ { case v => P(v, N) }
}

import parser._

object repl {
  def parse(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    e
  }
  def evl(n: String, e: Term): (Int => Int, String, String) = {
    trait Prog extends EvalProg {
      override def snippet(x: Rep[Int]): Rep[Int] = {
        base_eval(e, init_env, {v => v })
        apply_fun(n, List(x))
      }
    }
    val snippet = new Prog with EvalSnippet
    val snippetC = new Prog with EvalCSnippet
    (snippet.f, snippet.code, snippetC.code)
  }
  def evp(n: String, s: String) = evl(n, parse(s))
}

import ast._
import repl._
import utils._
class lisp0_Tests extends TestSuite {
  def ev(n: String, s: String): Int => Int = {
    val (f, scala_code, c_code) = evp(n, s)
    check(n, scala_code, suffix="scala")
    check(n, c_code, suffix="c")
    f
  }

  test("(factorial 6)") {
    val factorial = ev("factorial", """(begin
(define (factorial n) (if (< n 2) n (* n (factorial (- n 1)))))
)""")
    assert(720 == factorial(6))
  }

  test("(odd 7)") {
    val odd = ev("odd", """(begin
(define (even n) (if (= n 0) 1 (odd (- n 1))))
(define (odd n) (if (= n 0) 0 (even (- n 1))))
)""")
    assert(1 == odd(7))
  }
}
