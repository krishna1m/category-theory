package abstractmath

trait Functor[F[_]]:
  def map[A, B](a: F[A])(f: A => B): F[B]

object Functor:
  def apply[F[_]: Functor] = summon[Functor[F]]

extension [F[_]: Functor, A](fa: F[A])
  def map[B](f: A => B): F[B] = Functor[F].map(fa)(f)

