package lib

import cats.effect.std.Dispatcher
import cats.effect.kernel.Async
import cats.effect.IO
import lib.raise.Faillible
import lib.raise.Raise

/** From Valentin Bergeron : the poor man suspend
  *
  * ```
  * val foo = suspend:
  *   val a = IO.pure(2).await
  *   val b = IO.pure(3).await
  *   a + b
  * println(foo) // 5
  * ```
  */
trait SuspendOps:

  inline def suspend[A](
      thunk: Dispatcher[IO] ?=> A
  ): IO[A] =
    Dispatcher
      .sequential[IO]
      .use(dispatcher => IO.delay(thunk(using dispatcher)))

  extension [A](
      ioa: IO[A]
  )(using d: Dispatcher[IO]) inline def await: A = d.unsafeRunSync(ioa)

object suspend extends SuspendOps
