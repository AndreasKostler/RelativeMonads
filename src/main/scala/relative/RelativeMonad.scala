package com.goodlord.relative


trait RelativeMonad[R[_], M[_]] {
  def rPure[A](v: => R[A]): M[A]
  def rFlatMap[A, B](ma: M[A])(f: R[A] => M[B]): M[B]
}

/** Methods and trivial instances for relative monads. */
object RelativeMonad {
  type Id[A] = A

  /** Every monad has a trivial self relative instance `RelMonad[R, R]`. */
  class SelfR[R[_]] extends RelativeMonad[R, R] {
    def rPure[A](expr: => R[A]): R[A] = expr

    def rFlatMap[A, B](ra: R[A])(f: R[A] => R[B]): R[B] = f(ra)
  }

  /** Ordinary monads are relative to Id. Note `Id[A] = A` directly. */
  class IdR[R[_]](implicit R: Monad[R]) extends RelativeMonad[Id, R] {
    def rPure[A](expr: => Id[A]): R[A] = R.pure(expr)

    def rFlatMap[A, B](ra: R[A])(f: Id[A] => R[B]): R[B] = R.flatMap(ra)(f)
  }

  /** Relatives to Id are ordinary monads.  Note `Id[A] = A`. */
  class MonadIdR[R[_]](R: RelativeMonad[Id, R]) extends Monad[R] {
    def pure[A](expr: => A): R[A] = R.rPure(expr)

    def flatMap[A, B](ra: R[A])(f: Id[A] => R[B]): R[B] = R.rFlatMap(ra)(f)
  }

  /** Relative map, handy for lifting functions from M to N */
  def rMap[A, B, M[_], N[_]](na: N[A])(f: M[A] => M[B])(
    MrelN: RelativeMonad[M, N]
  ): N[B] = {
    MrelN.rFlatMap(na)(ma => MrelN.rPure(f(ma)))
  }
}