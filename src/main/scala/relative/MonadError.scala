package com.goodlord.relative

/**
  * A monad that also allows you to raise and or handle an error value.
  *
  * This type class allows one to abstract over error-handling monads.
  */
trait MonadError[M[_], E] {
  implicit val M: Monad[M]

  /**
    * Lift an error into the `M` context.
    */
  def raiseError[A](e: E): M[A]

  /**
    * Handle any error, potentially recovering from it, by mapping it to an
    * `M[A]` value.
    *
    */
  def handleErrorWith[A](fa: M[A])(f: E => M[A]): M[A]

  def pure[A](a: A): M[A] = M.pure(a)
}


object MonadError {

  def apply[M[_], E](implicit ME: MonadError[M, E]) = ME

  def ok[M[_], A, E](a: A)(implicit ME: MonadError[M, E]) = ME.pure(a)

  def error[M[_], A, E](e: E)(implicit ME: MonadError[M, E]): M[A] = ME.raiseError(e)

}