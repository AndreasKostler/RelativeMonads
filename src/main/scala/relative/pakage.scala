package com.goodlord

package object relative {

  trait Monad[F[_]] {
    def pure[A](a: => A): F[A]

    def flatMap[A, B](ma: F[A])(fa: A => F[B]): F[B]

    def map[A, B](ma: F[A])(fa: A => B): F[B] =
      flatMap(ma)(a => pure(fa(a)))
  }

  object Monad {
    def apply[F[_]](implicit M: Monad[F]): Monad[F] = M
  }

  type Result[A] = Either[String, A]

  final class ResultOps[A](r: Result[A]) {
    /**
      * Set the error message in a failure case.
      * Useful for providing contextual information without
      * having to inspect result.
      */
    def setMessage(m: String): Result[A] = Left(m)

    /**
      * Adds an additional error message.
      * Useful for adding more context as the error goes up the stack.
      */
    def addMessage(m: String): Result[A] = r.left.map(m0 => s"$m0 : $m")
  }

  trait ToResultOps {
    implicit def toResultOps[A](r: Result[A]): ResultOps[A] = new ResultOps(r)
  }

  object Result extends ToResultOps {
    def ok[A](a: A): Result[A] = Right(a)

    def fail[A](m: String): Result[A] = Left(m)

    implicit val resultMonad: Monad[Result] = new Monad[Result] {
      def pure[A](a: => A): Result[A] = Result.ok(a)

      def flatMap[A, B](ma: Result[A])(fa: A => Result[B]): Result[B] =
        ma.flatMap(fa)
    }

    implicit def resultMonadError: MonadError[Result, String] = new MonadError[Result, String] {
      implicit val M: Monad[Result] = resultMonad

      def raiseError[A](e: String): Result[A] = Result.fail(e)

      def handleErrorWith[A](ma: Result[A])(f: String => Result[A]): Result[A] = ma.fold(f, Result.ok)

    }
  }

  // Result ops implemented in terms of MonadError
  final class MonadResultOps[M[_], A](val self: M[A])(implicit M: MonadError[M, String]) {

    def handleError(f: String => M[A]): M[A] = M.handleErrorWith(self)(f)

    /**
      * Set the error message in a failure case. Useful for providing contextual information without
      * having to inspect result.
      *
      * NB: This discards any existing message.
      */
    def tSetMessage(message: String): M[A] = handleError(_ => M.raiseError(message))

    /**
      * Adds an additional error message. Useful for adding more context as the error goes up the stack.
      *
      * The new message is prepended to any existing message.
      */
    def tAddMessage(message: String, separator: String = ": "): M[A] =
      handleError(m => M.raiseError(s"${message}${separator}$m"))

  }

  trait ToMonadResultOps {
    /** Pimps a [[MonadError]] to have access to the functions in [[MonadResultOps]]. */
    implicit def ToMonadErrortOps[M[_], A](v: M[A])(implicit M0: MonadError[M, String]): MonadResultOps[M, A] =
      new MonadResultOps[M, A](v)
  }

  object MonadResultSyntax extends ToMonadResultOps
}