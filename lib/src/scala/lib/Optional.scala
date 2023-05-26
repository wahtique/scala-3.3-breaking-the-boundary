package lib

import scala.util.boundary.Label
import scala.util.boundary
import scala.util.boundary.break

trait OptionalOps:
  type Opt[A] = Label[None.type] ?=> A
  inline def optional[A](body: Opt[A]): Option[A] = boundary(Some(body))
  extension [A](oa: Option[A]) inline def ? : Opt[A] = oa.getOrElse(break(None))

object optional extends OptionalOps
