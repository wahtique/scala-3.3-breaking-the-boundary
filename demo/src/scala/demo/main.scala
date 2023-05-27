package demo

import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.effect.IOApp
import lib.all.*
import scala.concurrent.duration.*

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
  // suspend to avoid a for...yield block
  def getAge(name: String): IO[Either[ApiError, Int]] = suspend:
    val result = handle(Service.getAge(name)).await
    result match
      case Left(DomainError.NotAdmin(msg)) =>
        Left(ApiError.NotAuthorized(msg))
      case Left(DomainError.NotFoundInDB(msg)) => Left(ApiError.NotFound(msg))
      case Left(RepositoryError.Badaboum(msg)) =>
        Left(ApiError.InternalError(msg))
      case Right(age) => Right(age)

object Service:
  // `Raise(...)` specifies the type of error that can be raised by this function
  // do not here we could make this cleaner and raise only repository errors
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
      .fold(RepositoryError.Badaboum("Oopsie").!)(IO.pure)

// main

object App extends IOApp.Simple:
  override def run: IO[Unit] = suspend:
    // add some delay to simulate network latency
    // showcase sequenciallity
    IO.sleep(1.seconds).await
    val john = Controller.getAge("John").await
    IO.println(john).await // Right(42)
    IO.sleep(1.seconds).await
    val jane = Controller.getAge("Jane").await
    IO.println(jane).await // Left(io.wahtique.ApiError$InternalError: Oopsie)
    IO.sleep(2.seconds).await
    val bob = Controller.getAge("Bob").await
    IO.println(bob)
      .await // Left(io.wahtique.ApiError$NotAuthorized: Bob is not a known user)
