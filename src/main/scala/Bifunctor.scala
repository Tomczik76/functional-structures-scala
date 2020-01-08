import simulacrum._

@typeclass trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = 
    bimap(fab)(f, identity)

  def map[A, B, C, D](fab: F[A, B])(f: B => D): F[A, D] =
    bimap(fab)(identity, f)
}

object BifunctorInstances {
  implicit def eitherInstance = new Bifunctor[Either] {
    def bimap[A, B, C, D](either: Either[A, B])(f: A => C, g: B => D): Either[C, D] =
     either match {
       case Left(a) => Left(f(a))
       case Right(b) => Right(g(b))
     }
  }
}