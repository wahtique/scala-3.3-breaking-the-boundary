package lib

import scala.util.boundary.Label
import cats.effect.IO
import scala.util.boundary
import scala.util.boundary.break
import cats.implicits.*

/** Capability to throw an exception wrapped in a IO of Either.Left
  *
  * ```
  * final case class MathError(msg: String) extends Exception(msg)
  * def divide(a: Int, b: Int)(using Raise[MathError]): IO[Int] = ???
  * ```
  */
trait RaiseOps:

  type Raise[-E <: Throwable] = Label[IO[Left[E, Nothing]]]

  type Faillible[+E <: Throwable, A] = Raise[E] ?=> IO[A]

  def raise[E <: Throwable, A](e: E)(using Raise[E]): IO[A] =
    break(IO.pure(Left(e)))

  inline def handle[E <: Throwable, A](
      inline body: Raise[E] ?=> IO[A]
  ): IO[Either[E, A]] = boundary(
    body.attempt.asInstanceOf[IO[Either[E, A]]]
  )

  extension [E <: Throwable](e: E) def ![A]: Faillible[E, A] = raise(e)

  // todo make this work
  extension [A](ioa: IO[A])
    inline def ?[E <: Throwable]: IO[Either[E, A]] = handle(ioa)

object raise extends RaiseOps
