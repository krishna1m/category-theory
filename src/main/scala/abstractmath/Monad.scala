package abstractmath

import scala.util.{Try, Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global


trait Monad[M[_]] extends Functor[M]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))
end Monad

object Monad:
  def apply[M[_]: Monad] = summon[Monad[M]]
  def getPairs[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    Monad[M].flatMap(ma)((a: A) => Monad[M].map(mb)((b: B) => (a, b)))

extension [A](a: A)
  def pure[M[_]: Monad]: M[A] = Monad[M].pure(a)

extension [M[_]: Monad, A](ma: M[A])
  def flatMapE[B](f: A => M[B]): M[B] = Monad[M].flatMap(ma)(f)
  def mapE[B](f: A => B): M[B] = Monad[M].map(ma)(f)

type ErrorOr[A] = Either[String, A]

object MonadInstances:
  given Monad[Option] with
    def pure[A](a: A): Option[A] = Option(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match
      case Some(a) => f(a)
      case None => None

  given Monad[Try] with
    def pure[A](a: A): Try[A] = Try(a)
    def flatMap[A, B](ma: Try[A])(f: A => Try[B]): Try[B] = ma match
      case Success(a) => f(a)
      case Failure(ex) => Failure(ex)

  given Monad[ErrorOr] with
    def pure[A](a: A): ErrorOr[A] = Right(a)
    def flatMap[A, B](ma: ErrorOr[A])(f: A => ErrorOr[B]): ErrorOr[B] = ma match
      case Right(value) => f(value)
      case Left(error) => Left(error)

  given Monad[List] with
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma match
      case a :: as => f(a) ::: flatMap(as)(f)
      case Nil => Nil
end MonadInstances

object MonadPlayground:
  def main(args: Array[String]): Unit = 
    import MonadInstances.given
    val optionInt: Option[Int] = 4.pure[Option] // Some(4)
    val tryInt: Try[Int] = 4.pure[Try]
    val errorOrInt: ErrorOr[Int] = 4.pure[ErrorOr]
    val listInt: List[Int] = 4.pure[List]

    val optionChar: Option[Char] = 'a'.pure[Option] // Some('a')
    val tryChar: Try[Char] = 'a'.pure[Try]
    val errorOrChar: ErrorOr[Char] = 'a'.pure[ErrorOr]
    val listChar: List[Char] = 'a'.pure[List]

    val numbers = List(1, 2, 3)
    val chars = List('a', 'b', 'c')

    println(Monad.getPairs(optionInt, optionChar))
    println(Monad.getPairs(tryInt, tryChar))
    println(Monad.getPairs(errorOrInt, errorOrChar))
    println(Monad.getPairs(numbers, chars))
end MonadPlayground 
