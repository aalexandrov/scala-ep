package arithmetic

import common.Id

// Language Encoding
// -----------------

/* GADT */
enum Expr[A]:
  case Add[A: Numeric](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
  case Sub[A: Numeric](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
  case Mul[A: Numeric](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]

/* Böhm-Berarducci encoding (tagless final algebra) */
trait Alg[R[_]] {
  def add[A: Numeric](lhs: R[A], rhs: R[A]): R[A]
  def sub[A: Numeric](lhs: R[A], rhs: R[A]): R[A]
  def mul[A: Numeric](lhs: R[A], rhs: R[A]): R[A]
}

// Recursion Scheme
// -----------------

trait fold[R[_]](alg: Alg[R]) {
  def apply[A: Numeric](expr: Expr[A]): R[A] = expr match {
    case Expr.Add(lhs, rhs) =>
      alg.add(this(lhs), this(rhs))
    case Expr.Sub(lhs, rhs) =>
      alg.sub(this(lhs), this(rhs))
    case Expr.Mul(lhs, rhs) =>
      alg.mul(this(lhs), this(rhs))
  }
}

// Interpreters
// ------------

trait reflect extends Alg[Expr] {
  def add[A: Numeric](lhs: Expr[A], rhs: Expr[A]): Expr[A] =
    Expr.Add(lhs, rhs)

  def sub[A: Numeric](lhs: Expr[A], rhs: Expr[A]): Expr[A] =
    Expr.Sub(lhs, rhs)

  def mul[A: Numeric](lhs: Expr[A], rhs: Expr[A]): Expr[A] =
    Expr.Mul(lhs, rhs)
}

trait eval extends Alg[Id] {
  def add[A: Numeric](lhs: A, rhs: A): A = {
    val numeric = implicitly[Numeric[A]]
    numeric.plus(lhs, rhs)
  }

  def sub[A: Numeric](lhs: A, rhs: A): A = {
    val numeric = implicitly[Numeric[A]]
    numeric.minus(lhs, rhs)
  }

  def mul[A: Numeric](lhs: A, rhs: A): A = {
    val numeric = implicitly[Numeric[A]]
    numeric.times(lhs, rhs)
  }
}
