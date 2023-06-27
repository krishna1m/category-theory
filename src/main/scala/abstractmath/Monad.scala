package abstractmath

import scala.util.{Try, Success, Failure}

trait Monad[M[_]] extends Functor[M]:
  def pure[A](a: A): M[A] // `pure` comes from Applicative - to wrap a normal value into a monadic value
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // `flatMap` comes from FlatMap - to transform monadic values in sequence
  def map[A, B](ma: M[A])(f: A => B): M[B] = // `map` comes from Functor
    flatMap(ma)(a => pure(f(a)))
  def tailRecM[A, B](a: A)(f: A => M[Either[A, B]]): M[B] // needs to implemented for every custom Monad
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
    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match
      case Some(Right(b)) => Some(b)
      case Some(Left(newA)) => tailRecM(newA)(f)
      case None => tailRecM(a)(f)

  given Monad[Try] with
    def pure[A](a: A): Try[A] = Try(a)
    def flatMap[A, B](ma: Try[A])(f: A => Try[B]): Try[B] = ma match
      case Success(a) => f(a)
      case Failure(ex) => Failure(ex)
    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = f(a) match
      case Success(Right(b)) => Success(b)
      case Success(Left(newA)) => tailRecM(newA)(f)
      case Failure(_) => tailRecM(a)(f)

  given Monad[ErrorOr] with
    def pure[A](a: A): ErrorOr[A] = Right(a)
    def flatMap[A, B](ma: ErrorOr[A])(f: A => ErrorOr[B]): ErrorOr[B] = ma match
      case Right(value) => f(value)
      case Left(error) => Left(error)
    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => ErrorOr[Either[A, B]]): ErrorOr[B] = f(a) match
      case Right(Right(b)) => Right(b)
      case Right(Left(newA)) => tailRecM(newA)(f)
      case Left(_) => tailRecM(a)(f)

  given Monad[List] with
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma match
      case a :: as => f(a) ::: flatMap(as)(f)
      case Nil => Nil
    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = f(a) match
      case List(Right(b)) => List(b)
      case List(Left(newA)) => tailRecM(newA)(f)
      case Nil => tailRecM(a)(f)
end MonadInstances

object MonadPlayground:
  def main(args: Array[String]): Unit = 
    import MonadInstances.given
    val optionInt: Option[Int] = 4.pure[Option] // Some(4)
    val tryInt: Try[Int] = 4.pure[Try]
    val errorOrInt: ErrorOr[Int] = 3.pure[ErrorOr]
    val listInt: List[Int] = 4.pure[List]
    println(errorOrInt.flatMapE(n => if (n % 2 == 0) Right(n + 1) else Left("Provided input was odd!")))

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

object HttpServicePlayground extends App:
    import MonadInstances.given
    
    // Contrived Example
    case class Connection(host: String, port: String)
    val config = Map(
        "host" -> "localhost",
        "port" -> "4040"
      )

    trait HttpService[M[_]]:
      def getConnection(cfg: Map[String, String]): M[Connection]
      def issueRequest(connection: Connection, payload: String): M[String]

    object EitherHttpService extends HttpService[ErrorOr]:
      def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
        (cfg.get("host"), cfg.get("port")) match
          case (Some(host), Some(port)) => Right(Connection(host, port))
          case _ => Left("Connection could not be set up")
      def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
        if(payload.length <= 20) Right("payload sent")
        else Left("Message too large!")

    def getConAndSend(cfg: Map[String, String], payload: String): ErrorOr[String] =
      EitherHttpService.getConnection(cfg).flatMapE(con =>
        EitherHttpService.issueRequest(con, payload))

    def getConAndSendFor(cfg: Map[String, String], payload: String): ErrorOr[String] =
      for {
        con <- EitherHttpService.getConnection(config)
        response <- EitherHttpService.issueRequest(con, payload)
      } yield response

    val smallMessage = "How u doin'?"
    val largeMessage = "Hey there! How are you doing?"
    println(getConAndSend(config, smallMessage))
    println(getConAndSendFor(config, largeMessage))

end HttpServicePlayground

object CustomMonadPlayground extends App:
  type Identity[T] = T
  given Monad[Identity] with
    def pure[A](a: A): Identity[A] = a
    def flatMap[A, B](ma: Identity[A])(f: A => Identity[B]): Identity[B] = f(ma)
    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match
      case Left(newA) => tailRecM(newA)(f)
      case Right(b) => b

  sealed trait Tree[+T]
  final case class Leaf[+T](value: T) extends Tree[T]
  final case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

  given Monad[Tree] with
    def pure[A](a: A): Tree[A] = Leaf(a)
    def flatMap[A, B](ma: Tree[A])(f: A => Tree[B]): Tree[B] = ma match
      case Leaf(a) => f(a)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = 
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match
        case Leaf(Right(b)) => Leaf(b)
        case Leaf(Left(newA)) => stackRec(f(newA))
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      stackRec(f(a))
    

end CustomMonadPlayground
