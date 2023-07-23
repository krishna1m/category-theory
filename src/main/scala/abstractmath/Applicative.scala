package abstractmath

import scala.util.Try

trait Applicative[F[_]]:
  def pure[A](a: A): F[A]

object Applicative:
  def apply[F[_]: Applicative]: Applicative[F] = summon[Applicative[F]]

extension [A](a: A)
  def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(a)

object ApplicativeInstances:
  given Applicative[List] with
    def pure[A](a: A): List[A] = List(a)
  given Applicative[Option] with
    def pure[A](a: A): Option[A] = Option(a)
  given Applicative[Try] with
    def pure[A](a: A): Try[A] = Try(a)

object ApplicativePlayground extends App:
  import ApplicativeInstances.given
  val listOfInt = 4.pure[List]
  val optionOfInt = 4.pure[Option]
  val tryOfInt = 4.pure[Try]
  println(listOfInt)
  println(optionOfInt)
  println(tryOfInt)
