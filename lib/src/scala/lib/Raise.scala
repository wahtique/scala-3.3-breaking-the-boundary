package lib

import scala.util.boundary.Label
import cats.effect.IO
import scala.util.boundary
import scala.util.boundary.break
import cats.implicits.*

/** Capacity to throw an exception wrapped in a IO of Either.Left
  *
  * ```
  * final case class MathError(msg: String) extends Exception(msg)
  * def divide(a: Int, b: Int)(using Raise[MathError]): IO[Int] = ???
  * ```
  */

object raise:

  type Raise[-E <: Throwable] = Label[IO[Left[E, Nothing]]]

  type Faillible[+E <: Throwable, A] = Raise[E] ?=> IO[A]

  def raise[E <: Throwable, A](e: E)(using Raise[E]): IO[A] =
    break(IO.pure(Left(e)))

  inline def faillible[E <: Throwable, A](
      inline body: Raise[E] ?=> IO[A]
  ): IO[Either[E, A]] = boundary(
    body.attempt.asInstanceOf[IO[Either[E, A]]]
  )
