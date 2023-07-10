package abstractmath

import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

object Functor:
  def apply[F[_]: Functor]: Functor[F] = summon[Functor[F]]

extension [F[_]: Functor, A](fa: F[A])
  def mapE[B](f: A => B): F[B] = Functor[F].map(fa)(f)

object FunctorInstances:
  given Functor[List] with
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  given Functor[Option] with
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  given Functor[Try] with
    def map[A, B](fa: Try[A])(f: A => B): Try[B] =
      fa.map(f)
  given eitherFunctor[E]: Functor[[A] =>> Either[E, A]] with // type lambdas in Scala 3
    def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa match
      case Left(err) => Left(err)
      case Right(value) => Right(f(value))

sealed trait Tree[+T]
case class Leaf[+T](value: T) extends Tree[T]
case class Branch[+T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]

object Tree:
  def leaf[T](value: T): Tree[T] = Leaf(value)
  def branch[T](leftT: Tree[T], value: T, rightT: Tree[T]): Tree[T] = 
    Branch(leftT, value, rightT)
  given Functor[Tree] with
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match
      case Leaf(value) => Leaf(f(value))
      case Branch(leftT, value, rightT) => 
        Branch(map(leftT)(f), f(value), map(rightT)(f))


object FunctorPlayground extends App:
  import FunctorInstances.given
  val numbers = List(1, 2, 3)
  println(Functor[List].map(numbers)(_ + 1))
  println(numbers.mapE(_ + 1))

  val intOption = Option(1)
  println(Functor[Option].map(intOption)(_ + 1))
  println(intOption.mapE(_ + 1))

  val aTry = Try(1)
  println(Functor[Try].map(aTry)(_ + 1))
  println(aTry.mapE(_ + 1))

  val myTree = Tree.branch(Tree.branch(Tree.leaf(4), 2, Tree.leaf(5)), 1, Tree.branch(Tree.leaf(6), 3, Tree.leaf(7)))
  val doubledTree = myTree.mapE(_ * 2)
  println(doubledTree)

  val anEither: Either[String, Int] = Right(2)
  println(anEither.map(_ + 1))

  val anErrorEither: Either[String, Int] = Left("Something bad happened but I don't know what!")
  println(anErrorEither.map(_ + 1))



  
