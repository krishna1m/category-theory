package abstractmath

import scala.util.Try

trait FlatMap[F[_]] extends Functor[F]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

object FlatMap:
  def apply[F[_]: FlatMap]: FlatMap[F] = summon[FlatMap[F]]

extension [F[_]: FlatMap, A](fa: F[A])
  def flatMapE[B](f: A => F[B]): F[B] = FlatMap[F].flatMap(fa)(f)

object FlatMapInstances:
  given FlatMap[List] with
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  given FlatMap[Option] with
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  given FlatMap[Try] with
    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
      fa.flatMap(f)
    def map[A, B](fa: Try[A])(f: A => B): Try[B] =
      fa.map(f)

object FlatMapPlayground extends App:
  import FlatMapInstances.given
  val listOfInts = List(1, 2, 3)
  val flatMappedListOfInts = listOfInts.flatMapE(x => List(x, x + 1))

  val optionOfInt = Option(1)
  val flatMappedOptionOfInt = optionOfInt.flatMapE(x => Option(2 * x))

  val tryOfInt = Try(1)
  val flatMappedTryOfInt = tryOfInt.flatMapE(x => Try(2 * x))

  println(flatMappedListOfInts)
  println(flatMappedOptionOfInt)
  println(flatMappedTryOfInt)
