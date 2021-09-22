type Alg[R[_]] = core.Alg[R] & arithmetic.Alg[R]
type Expr[A] = core.Expr[A] | arithmetic.Expr[A]

object eval extends core.eval, arithmetic.eval

class fold[R[_]](alg: Alg[R]) extends core.fold[R](alg), arithmetic.fold[R](alg)

object fold {
  def apply[R[_]](alg: Alg[R]) = new fold(alg)
}

object terms {

  def t1[R[_]](alg: Alg[R]) =
    alg.ifelse(
      alg.lit(true),
      alg.lit(5),
      alg.lit(4)
    )

  def t2[R[_]](alg: Alg[R]) =
    alg.sub(
      alg.lit(5),
      alg.mul(
        alg.lit(5),
        alg.lit(4)
      )
    )
}

@main def hello: Unit = {
  val a: Expr[Int] = core.Expr.Lit(1)

  // Q: how to construct an instance of Alg[R]?
  // works fine when R = Id, but having problems when R = Expr (a true HKT)

  // println("eval reflected:")
  // println( /* fold(eval) */ (terms.t1(new reflect)))

  // val x: Alg[Expr] = new println("eval directly:")
  println(terms.t1(eval))
}
