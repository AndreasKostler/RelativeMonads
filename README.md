# Relative Monads for Fun and Profit
Throughout my career I was blessed with working alongside some of the brightest minds in the industry - witnessing these minds at work can be a humbling and incredibly educational experience.

Back in 2015, during my tenure at Commonwealth Bank of Australia, two of my colleagues Stefan Hoermann and Rowan Davies 'stumbled upon' a simplified version of the `RelativeMonad` through a series of refactorings and subsequently applied theoretical rigour and distil it into a LambdaJam [talk and workshop](http://yowconference.com.au/slides/yowlambdajam2015/Davies-Hoermann-RelativeMonads.pdf).
Unfortunately, I was unable to attend lambda yam that year (highly recommended) so I was left somewhat unsatisfied - I didReln't fully understand the weight of Stefan and Rowan's contribution, nor did I fully understand the process of discovery and the potential applications of the `RelativeMonad` at the time.

The `RelativeMonad` has haunted me ever since: I studied the theory, the talk, the code accompanying the LambdaJam workshop, and production usage. I always felt I was missing a vital piece of information, the piece that would make it all click, the piece that would help me understand why this matters, the Eureka, the Aha. I understood the `how` but I was left with a question in my head: So what?

With this blog post I want to fill this gap: Not only do I want to show how the `RelativeMonad` naturally arises from generalising `MonadError` but I will also some applications beyond 'a more general MonadError' - I will leave the context of error handling behind and relate two less obvious Monads. I will show how the `RelativeMonad` can be used in conjunction with and instead of monad transformers.

So, let's get straight into it :)

## Deriving the Relative Monad
Stefan and Davies, were at the time working on frameworks for doing [ETL](https://en.wikipedia.org/wiki/Extract,_transform,_load) kind of work on a Hadoop platform. Naturally, they were dealing with local file systems, HDFS, and several databases. While inherently different, all share one common attribute - operations on them can fail, i.e. they need to be able to handle error values.

### Filesystem Example
Our first example models a simple file system class defining some basic operations on a local file system. File system operations can fail and ideally our file system class would provide ways of raising and handling these errors. Let's look at some code, shall we?

The result type we will be using throughout this post is:
```scala
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
    def pure[A](a: A): Result[A] = Result.ok(a)

    def flatMap[A, B](ma: Result[A])(fa: A => Result[B]): Result[B] =
      ma.flatMap(fa)
  }
}
```

And the initial version of our file system class is:
```scala
case class FS[A](runFS: File => Result[A]) {
  def runPath(s: String): Result[A] = runFS(new File(s))

  def setMessage(message: String): FS[A] =
    FS((f: File) => runFS(f).setMessage(message))

  def addMessage(message: String): FS[A] =
    FS((f: File) => runFS(f).addMessage(message))
}

object FS {
  def fs[A](a: => A): FS[A] =
    FS(_ => Result.ok(a))

  def listFiles: FS[List[String]] =
    FS(f => Result.ok(f.list).map(_.toList))

  def ls: FS[List[String]] =
    listFiles.setMessage("Invalid Path")

  def rm: FS[Unit] = ???

  // etc...
}
```

The methods `setMessage` and `addMessage` provide ways of augmenting the file system operations with error messages. As mentioned in the introduction, we not only have file systems but also operations on databeses, HDFS, etc. All of them support the same error handling functionality.

How do we express this without duplicating the implementations for `setMessage` and `addMessage`?
Luckily, there is a way of expressing error handling behaviour and abstracting over error handling monads by means of the `MonadError` type class:

```scala
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

// Monad error instance for Result
implicit def resultMonadError: MonadError[Result, String] = new MonadError[Result, String] {
  implicit val M: Monad[Result] = resultMonad

  def raiseError[A](e: String): Result[A] = Result.fail(e)

  def handleErrorWith[A](ma: Result[A])(f: String => Result[A]): Result[A] = ma.fold(f, Result.ok)
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
```

By providing a `MonadError` instance for `FS` we add error handling functionality to `FS`. More generally, by providing a `MonadError` instance for any monad `M` we add error handling capability to `M`.

Let's pause here for a moment and reflect on what we've done so far. By pulling out error handling functionality into the `MonadError` type class we were able to inject functions defined on `Result` in terms of `raiseError` and `handleError` into our file system class. `raiseError` lifts an error `E` into our `FS` class by means of the `Result` constructor. `handleError` combines computations that can fail - again by lifting `E` into `FS`, this time by means of running the provided function `f`.
Therefore, by providing a path from our result type `Result` to `FS` we get methods defined on `Result` for free.

Now this is where most developers will stop - and indeed if you got his far, please [apply](https://www.workable.com/j/A3154D4BC7). However, Stefan and Rowan pushed the abstraction game a little further. Instead of abstracting over our error type `E`, we could abstract over any monad `R`. If we mechanically replace `E` with `R` our `MonadError` type class becomes:

```scala
trait MonadR[M[_], R[_]] {
  def raiseR[A](e: R[A]): M[A]

  def handleR[A,B](ma: M[A])(f: R[A] => M[B]): M[B]
}
```
`raiseR` lifts our generic effect `R` into our target monad `M`, `handleR` combines `R`-effectful computations on `M`. If we substitute `Result` for `R` we obtain our original `MonarError` instance.
Let's swap the `M` and `R` type parameters and rename the operations and we arrive at the definition of the `RelativeMonad` type class.
```scala
trait RelativeMonad[R[_], M[_]] {
  def rPoint[A](v: => R[A]): M[A]

  def rBind[A, B](ma: M[A])(f: R[A] => M[B]): M[B]
}
```

The structure naturally derives from `MonadError`. In words, the `RelativeMonad` type class allows you to raise and handle an arbitrary (effect) `R`. It allows you to abstract over `R`-handling monads. This sounds very abstract so let's look at our initial example again. Firstly, we need to implement our `Result` operations in terms of the relative monad:

```scala
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
```

Then we can re-implement the file system operations using `rSetMessage` and `rAddMessage` define in `RelResultOps` like so:

```scala
object FS extends ToRelResultOps {
  def fs[A](a: => A): FS[A] =
    FS(_ => Result.ok(a))

  def listFiles: FS[List[String]] =
    FS(f => Result.ok(f.list).map(_.toList))

  def ls: FS[List[String]] =
    listFiles.rSetMessage("Invalid Path")

  def rm: FS[Unit] = ???

  // etc...

  implicit val relativeMonad: RelativeMonad[Result, FS] = new RelativeMonad[Result, FS] {
    def rPure[A](v: => Result[A]): FS[A] = FS(_ => v)

    def rFlatMap[A, B](ma: FS[A])(f: (Result[A]) => FS[B]): FS[B] =
      FS(x => f(ma.runFS(x)).runFS(x))

  }
}
```

So there you have it, error handling functionality by means of `RelativeResult`. But what if we free ourselves from `Result` completely? The `RelativeMonad` allows us to abstract over any `R` - as long as our monad provides a `RelativeMonad` instance for `R`.

## Discussion
With this post we showed how the relative monad derives naturally from `MonadError` by means of abstraction providing cheap syntax by relating to arbitrary monads. They form a viable alternative to monad transformers.

The full sources for this post are on [github](...)

## Outlook
In part 2 of this series we will dive a little deeper into the theory of relative monads, their relation to monad transformers, the relative monad laws and applications.
