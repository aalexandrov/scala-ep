package core

import common.Id

// Language Encoding
// -----------------

/* GADT */
enum Expr[A]:
  case Lit[A](value: A) extends Expr[A]
  case IfElse(cond: Expr[Boolean], thn: Expr[A], els: Expr[A])

/* Böhm-Berarducci encoding (tagless final algebra) */
trait Alg[R[_]] {
  def lit[A](value: A): R[A]
  def ifelse[A](cond: R[Boolean], thn: R[A], els: R[A]): R[A]
}

// Recursion Scheme
// -----------------

trait fold[R[_]](alg: Alg[R]) {
  def apply[A](expr: Expr[A]): R[A] = expr match {
    case Expr.Lit(value) =>
      alg.lit(value)
    case Expr.IfElse(cond, thn, els) =>
      alg.ifelse(this(cond), this(thn), this(els))
  }
}

// Interpreters
// ------------

trait reflect extends Alg[Expr] {
  def lit[A](value: A): Expr[A] =
    Expr.Lit(value)

  def ifelse[A](cond: Expr[Boolean], thn: Expr[A], els: Expr[A]): Expr[A] =
    Expr.IfElse(cond, thn, els)
}

trait eval extends Alg[Id] {
  def lit[X](value: X): X =
    value

  def ifelse[A](cond: Boolean, thn: A, els: A): A =
    if (cond) thn
    else els
}
