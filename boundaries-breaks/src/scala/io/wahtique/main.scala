package io.wahtique

import scala.util.boundary
import scala.util.boundary.Label
import scala.util.boundary.break
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.effect.IOApp

// quik n dirty domain model

enum ApiError(message: String) extends Exception(message):
  case NotAuthorized(message: String) extends ApiError(message)

enum DomainError(message: String) extends Exception(message):
  case NotAdmin(message: String) extends DomainError(message)
  case NotFoundInDB(message: String) extends DomainError(message)

// error handling

type CanFail[E <: Throwable] = Label[IO[Left[E, Nothing]]]

object CanFail:
  inline def apply[E <: Throwable, A](
      inline body: CanFail[E] ?=> IO[Either[E, A]]
  ): IO[Either[E, A]] = boundary(body)

def fail[E <: Throwable, A](e: E)(using CanFail[E]): IO[A] = break(IO(Left(e)))

// simple example using a fake webapp

object Config:
  val users = List("John", "Jane")

// Imagine a tapir endpoint somewhere else
object Controller:
  def getAge(name: String): IO[Either[ApiError, Int]] =
    for
      result <- CanFail(Service.getAge(name).attempt)
      response = result match
        case Left(e)    => Left(ApiError.NotAuthorized(e.getMessage))
        case Right(age) => Right(age)
    yield response

object Service:
  def getAge(name: String)(using CanFail[DomainError]): IO[Int] =
    if Config.users.contains(name) then Repository.getAgeFromRepo(name)
    else fail(DomainError.NotAdmin(s"$name is not a known user"))

object Repository:

  private val data = Map("John" -> 42)

  def getAgeFromRepo(name: String)(using CanFail[DomainError]): IO[Int] =
    data
      .get(name)
      .fold(
        fail[DomainError, Int](
          DomainError.NotFoundInDB(s"$name not found in DB")
        )
      )(IO.pure)

// main

object App extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      john <- Controller.getAge("John")
      _ <- IO.println(john) // Right(42)
      jane <- Controller.getAge("Jane")
      _ <- IO.println(jane) // Left(io.wahtique.ApiError$NotAuthorized: Jane not found in DB)
      bob <- Controller.getAge("Bob")
      _ <- IO.println(bob) // Left(io.wahtique.ApiError$NotAuthorized: Bob is not a known user)
    yield ()
