package com.goodlord.relative

object RelResult {
  @inline def apply[R[_]](implicit RM: RelativeMonad[Result, R]): RelativeMonad[Result, R] = RM
}


final class RelResultOps[R[_], A](val self: R[A])(implicit RM: RelativeMonad[Result, R]) {

  import Result._

  val M = Monad[Result]

  def rMap[B](f: Result[A] => Result[B]): R[B] =
    RM.rFlatMap(self)(r => RM.rPure(f(r)))

  def rFlatMap[B](f: Result[A] => R[B]): R[B] =
    RM.rFlatMap(self)(r => f(r))

  def rSetMessage(message: String): R[A] =
    rMap[A](_.setMessage(message))

  def rAddMessage(message: String): R[A] =
    rMap[A](_.addMessage(message))

}

trait ToRelResultOps {
  implicit def ToRelResultOps[R[_], A](v: R[A])(implicit M0: RelativeMonad[Result, R]): RelResultOps[R, A] =
    new RelResultOps[R, A](v)
}

object RelResultSyntax extends ToRelResultOps