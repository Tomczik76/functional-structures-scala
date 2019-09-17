import simulacrum._

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A], fn: A => B): F[B]
}
