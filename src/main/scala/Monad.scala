import simulacrum._

@typeclass trait Monad[F[_]] extends Applicative[F] {
  def flatten[A](ffa: F[F[A]]):F[A]
  
  def flatMap[A, B](fa: F[A])(fn: A => F[B]): F[B] =
    flatten(map(fa)(fn))
  
  override def map[A, B](fa: F[A])(fn: A => B): F[B] =
    flatMap(fa)((a:A) => pure(fn(a)))
  
  override def map2[A, B, C](fa:F[A], fb: F[B])(fn: (A, B) => C): F[C] =
   flatMap(fa)(a => map(fb)(b => fn(a, b)))
}

object MonadInstances {
  implicit def optionInstance[A] = new Monad[Option] {
    def flatten[A](ffa: Option[Option[A]]): Option[A] = 
      ffa match {
        case None => None
        case Some(Some(a)) => Some(a)
        case Some(None) => None
      }
    def pure[A](a: A): Option[A] = Some(a)
  }

  implicit def listInstances[A] = new Monad[List] {
    def flatten[A](ffa: List[List[A]]): List[A] =
      ffa match {
        case Nil => Nil
        case head :: tail => head ++ flatten(tail)
      }
      def pure[A](a: A): List[A] = a :: Nil
  }

  import Id._
  implicit val idInstance = new Monad[Id] {
    def flatten[A](a: A): Id[A] = a
    def pure[A](a:A):Id[A] = a
  }
}