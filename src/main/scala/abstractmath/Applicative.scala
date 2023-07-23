package abstractmath

import scala.util.Try

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]

object Applicative:
  def apply[F[_]: Applicative]: Applicative[F] = summon[Applicative[F]]

extension [A](a: A)
  def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(a)

object ApplicativeInstances:
  given Applicative[List] with
    def pure[A](a: A): List[A] = List(a)
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  given Applicative[Option] with
    def pure[A](a: A): Option[A] = Option(a)
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  given Applicative[Try] with
    def pure[A](a: A): Try[A] = Try(a)
    def map[A, B](fa: Try[A])(f: A => B): Try[B] =
      fa.map(f)

object ApplicativePlayground extends App:
  import ApplicativeInstances.given
  val listOfInt = 4.pure[List]
  val optionOfInt = 4.pure[Option]
  val tryOfInt = 4.pure[Try]
  println(listOfInt)
  println(optionOfInt)
  println(tryOfInt)
