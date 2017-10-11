package com.goodlord.relative


import java.io.File

case class FST[A](runFS: File => Result[A])

object FST {

  def ok[A](a: => A) = Monad[FST].pure(a)

  def error[A](e: String): FST[A] = MonadError.error[FST, A, String](e)(fstMonadError)

  def fs[A](a: => A): FST[A] =
    FST(_ => Result.ok(a))

  def listFiles: FST[List[String]] =
    FST(f => Result.ok(f.list).map(_.toList))

  /** List files but with a nice error message using MonadError functions. */

  def ls: FST[List[String]] =
    listFiles.tSetMessage("Invalid Path")

  def rm: FST[Unit] = ???

  implicit val fstMonadError = new MonadError[FST, String] {
    implicit val M: Monad[FST] = fstMonad
    implicit val MR: MonadError[Result, String] = MonadError[Result, String]

    def raiseError[A](e: String): FST[A] = FST.error(e)

    def handleErrorWith[A](ma: FST[A])(fa: String => FST[A]): FST[A] =
      FST[A](r => MR.handleErrorWith(ma.runFS(r))(x => fa(x).runFS(r)))
  }

  implicit val fstMonad: Monad[FST] = new Monad[FST] {
    def pure[A](a:  => A): FST[A] = FST.fs(a)

    def flatMap[A, B](ma: FST[A])(fa: A => FST[B]): FST[B] =
      FST[B]((cwd: File) => ma.runFS(cwd).flatMap(a => fa(a).runFS(cwd)))
  }

  implicit def FSTMonadResultOps[A](v: FST[A]): MonadResultOps[FST, A] =
    new MonadResultOps[FST, A](v)
}