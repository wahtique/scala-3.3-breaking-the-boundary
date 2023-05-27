package lib

import munit.FunSuite
import scopes.optional.*
import lib.scopes.optional

class OptionalSpecs extends FunSuite:

  test("traverse OK list of int as string => Some(list as int)"):
    val result = optional:
      List("1", "2", "3").map(_.toIntOption.?)
    assertEquals(result, Some(List(1, 2, 3)))

  test("traverse KO list of int as string => None"):
    val result = optional:
      List("1", "2", "3", "a").map(_.toIntOption.?)
    assertEquals(result, None)
