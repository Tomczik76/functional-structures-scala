import simulacrum._

@typeclass trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A], fn: A => F[B]): F[B]
  override def map[A, B](fa: F[A], fn: A => B): F[B] = flatMap(fa, (a:A) => pure(fn(a)))
}
