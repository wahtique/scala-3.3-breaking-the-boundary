package lib

import cats.effect.std.Dispatcher
import cats.effect.kernel.Async
import cats.effect.IO

/** From Valentin Bergeron : the poor man suspend
  *
  * ```
  * val foo = suspend:
  *   val a = run(IO.pure(2))
  *   val b = run(IO.pure(3))
  *   a + b
  * println(foo) // 5
  * ```
  */
trait SuspendOps:

  inline def run[A, F[_]](io: F[A])(using d: Dispatcher[F]): A =
    d.unsafeRunSync(io)

  inline def suspend[A, F[_]](
      thunk: Dispatcher[F] ?=> A
  )(using Async[F]): F[A] =
    Dispatcher
      .sequential[F]
      .use(dispatcher => Async[F].delay(thunk(using dispatcher)))

object suspend extends SuspendOps
