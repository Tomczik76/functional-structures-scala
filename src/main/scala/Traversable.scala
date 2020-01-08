import simulacrum._

@typeclass trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def flatSequence[G[_], A](fgfa: F[G[F[A]]])(implicit G: Applicative[G], F: Monad[F]): G[F[A]] =
    G.map(sequence(fgfa))(F.flatten)

  import Id._
  import ApplicativeInstances._
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)
}