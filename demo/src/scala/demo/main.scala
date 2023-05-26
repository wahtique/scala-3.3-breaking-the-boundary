package demo

import scala.util.boundary
import scala.util.boundary.Label
import scala.util.boundary.break
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.effect.IOApp
import lib.raise.*

// quik n dirty domain model

enum ApiError(message: String) extends Exception(message):
  case NotFound(message: String) extends ApiError(message)
  case NotAuthorized(message: String) extends ApiError(message)
  case InternalError(message: String) extends ApiError(message)

enum DomainError(message: String) extends Exception(message):
  case NotAdmin(message: String) extends DomainError(message)
  case NotFoundInDB(message: String) extends DomainError(message)

enum RepositoryError(message: String) extends Exception(message):
  case Badaboum(message: String) extends RepositoryError(message)

// simple example using a fake webapp

object Config:
  val users = List("John", "Jane")

// Imagine a tapir endpoint somewhere else
object Controller:
  def getAge(name: String): IO[Either[ApiError, Int]] =
    for
      // `faillible` here labelise this branch of code as being allowed to raise certain errors
      // in eithers that should be handled somehow
      result <- faillible(Service.getAge(name))
      response = result match
        case Left(DomainError.NotAdmin(msg)) =>
          Left(ApiError.NotAuthorized(msg))
        case Left(DomainError.NotFoundInDB(msg)) => Left(ApiError.NotFound(msg))
        case Left(RepositoryError.Badaboum(msg)) =>
          Left(ApiError.InternalError(msg))
        case Right(age) => Right(age)
    yield response

object Service:
  // `Raise(...)` specifies the type of error that can be raised by this function
  def getAge(
      name: String
  )(using Raise[DomainError | RepositoryError]): IO[Int] =
    if Config.users.contains(name) then Repository.getAgeFromRepo(name)
    else raise(DomainError.NotAdmin(s"$name is not a known user"))

object Repository:

  private val data = Map("John" -> 42)

  // Syntax using a "bifunctor"
  // eqv to `def getAgeFromRepo(name: String)(using Raise[E]): IO[Int]`
  def getAgeFromRepo(name: String): Faillible[RepositoryError, Int] =
    data
      .get(name)
      .fold(
        raise(
          RepositoryError.Badaboum("Oopsie")
        )
      )(IO.pure)

// main

object App extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      john <- Controller.getAge("John")
      _ <- IO.println(john) // Right(42)
      jane <- Controller.getAge("Jane")
      _ <- IO.println(
        jane
      ) // Left(io.wahtique.ApiError$NotAuthorized: Jane not found in DB)
      bob <- Controller.getAge("Bob")
      _ <- IO.println(
        bob
      ) // Left(io.wahtique.ApiError$NotAuthorized: Bob is not a known user)
    yield ()
