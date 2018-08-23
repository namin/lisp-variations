package lispc

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
  case class F(f: Value => Value) extends Value               // Functions
  case class Fexpr(f: Value => Value) extends Value           // Fexpr

  // Env is a list of frames (each a list of key/value pairs)
  // We use object structures for easy reification/reflection.
  type Env = P
  // Similarly, contitnuations are values too...
  type Cont = F

  def list(e: Value): List[Value] = e match {
    case N => Nil
    case P(first, rest) => first :: list(rest)
  }
  def valueOf(es: List[Value]): Value = es match {
    case Nil => N
    case first::rest => P(first, valueOf(rest))
  }
  def fexprOf(f: (Value, Env, Cont) => Value) = Fexpr{ v => v match {
    case P(exp, P(env:P, P(cont:F, N))) => f(exp, env, cont)
  }}
}

import ast._
object eval {
  def base_eval(exp: Value, env: Env, cont: Cont): Value = {
    exp match {
      case I(_) | B(_) => cont.f(exp)
      case S(sym) => eval_var(exp, env, cont)
      case P(fun, args) => base_apply(exp, env, cont)
    }
  }

  def base_apply(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(fun, args) => base_eval(fun, env, F{ vf => vf match {
      case F(f) => evlist(args, env, F{ vas => cont.f(f(vas)) })
      case Fexpr(f) => f(P(exp, P(env, P(cont, N))))
    }})
  }

  def eval_var(exp: Value, env: Env, cont: Cont): Value = exp match {
    case S(x) => cont.f(get(env, x))
  }

  def eval_quote(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(x, N)) => cont.f(x)
  }

  def eval_if(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(c, P(a, P(b, N)))) => base_eval(c, env, F{ cv => cv match {
      case B(false) => base_eval(b, env, cont)
      case B(true) => base_eval(a, env, cont)
    }})
  }

  def eval_set_bang(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(S(x), P(rhs, N))) => base_eval(rhs, env, F{ v =>
      cont.f(set(env, x, v))
    })
  }

  def eval_lambda(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(params, body)) => cont.f(F({args =>
      eval_begin(body, extend(env, params, args), F{v => v})
    }))
  }

  def eval_begin_exp(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, body) =>  eval_begin(body, env, cont)
  }
  def eval_begin(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(e, N) => base_eval(e, env, cont)
    case P(e, es) => base_eval(e, env, F{ _ => eval_begin(es, env, cont) })
  }

  def eval_define(exp: Value, env: Env, cont: Cont): Value = exp match {
    case P(_, P(r@S(name), body)) => {
      val p = P(r,Undefined)
      env.car = P(p, env.car)
      eval_begin(body, env, F{v =>
        p.cdr = v
        cont.f(r)})
    }
  }

  def evlist(exp: Value, env: Env, cont: Cont): Value = exp match {
    case N => cont.f(N)
    case P(first, rest) => base_eval(first, env, F{v => evlist(rest, env, F{vs => cont.f(P(v,vs))})})
  }

  def extend(env: Env, params: Value, args: Value): Env = {
    val frame = valueOf((list(params) zip  list(args)).map{t => P(t._1, t._2)})
    P(frame, env)
  }

  def find(frame: Value, x: String): Option[P] = frame match {
    case N => None
    case P(P(S(y),_), _) if (x==y) => Some(frame.asInstanceOf[P].car.asInstanceOf[P])
    case P(_, rest) => find(rest, x)
  }

  def get(env: Env, x: String): Value = env match {
    case P(first,rest) => find(first, x) match {
      case Some(P(k,v)) => v
      case None => get(rest.asInstanceOf[Env], x)
    }
  }

  def set(env: Env, x: String, v: Value): Value = env match {
    case P(first,rest) => find(first, x) match {
      case Some(p) => {
        p.cdr = v
        v
      }
      case None => get(rest.asInstanceOf[Env], x)
    }
  }

  def init_env: Env = P(valueOf(List(
    P(S("<"),   F({args => args match { case P(I(a), P(I(b), N)) => B(a<b) }})),
    P(S("*"),   F({args => args match { case P(I(a), P(I(b), N)) => I(a*b) }})),
    P(S("-"),   F({args => args match { case P(I(a), P(I(b), N)) => I(a-b) }})),
    P(S("eq?"), F({args => args match { case P(a, P(b, N)) => B(a==b) }})),
    P(S("quote"), fexprOf(eval_quote)),
    P(S("if"), fexprOf(eval_if)),
    P(S("set!"), fexprOf(eval_set_bang)),
    P(S("lambda"), fexprOf(eval_lambda)),
    P(S("begin"), fexprOf(eval_begin_exp)),
    P(S("define"), fexprOf(eval_define))
  )), N)
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
  def evl(e: Value) = { base_eval(e, global_env, F{ v => v } ) }
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
class lispc_Tests extends TestSuite {
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
