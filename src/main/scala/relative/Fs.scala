package com.goodlord.relative

import java.io.File

import com.goodlord.relative.Result._

case class FS[A](runFS: File => Result[A]) {
  def runPath(s: String): Result[A] = runFS(new File(s))

  def setMessage(message: String): FS[A] =
    FS((f: File) => runFS(f).setMessage(message))

  def addMessage(message: String): FS[A] =
    FS((f: File) => runFS(f).addMessage(message))
}

object FS extends ToRelResultOps {
  def fs[A](a: => A): FS[A] =
    FS(_ => Result.ok(a))

  def listFiles: FS[List[String]] =
    FS(f => Result.ok(f.list).map(_.toList))

  def ls: FS[List[String]] =
    listFiles.rSetMessage("Invalid Path")

  def rm: FS[Unit] = ???

  // etc...

  implicit val relMonad: RelativeMonad[Result, FS] = new RelativeMonad[Result, FS] {
    def rPure[A](v: => Result[A]): FS[A] = FS(_ => v)

    def rFlatMap[A, B](ma: FS[A])(f: (Result[A]) => FS[B]): FS[B] =
      FS(x => f(ma.runFS(x)).runFS(x))

  }
}