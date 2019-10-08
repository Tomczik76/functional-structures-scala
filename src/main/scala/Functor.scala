import simulacrum._

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(fn: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
  def void[A](fa: F[A]): F[Unit] = as(fa, ())
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => (a, f(a)))
  def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(a => (b, a))
}

object FunctorInstances {
  val optionInstance = new Functor[Option] {
    def map[A, B](opt: Option[A])(fn: A => B): Option[B] =
      opt match {
        case None => None
        case Some(a) => Some(fn(a))
      }
  }

  val listInstance = new Functor[List] {
    def map[A, B](list: List[A])(fn: A => B): List[B] =
      list match {
        case Nil => Nil
        case head :: tail => fn(head) :: map(tail)(fn)
      }
  }

  def eitherInstance[A] = new Functor[Either[A, *]] {
    def map[B, C](either: Either[A, B])(fn: B => C): Either[A, C] = {
      either match {
        case Left(a) => Left(a)
        case Right(b) => Right(fn(b))
      }
    }
  }
}