import simulacrum._

@typeclass trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, fa)((a, _) => f(a))

  @op("<*>")  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    map2(ff, fa)((f, a) => f(a))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  @op("*>") def productL[A, B](fa: F[A])(fb: F[B]): F[B] =
    map2(fa, fb)((_, b) => b)

  @op("<*" )def productR[A, B](fa: F[A])(fb: F[B]): F[A] =
    map2(fa, fb)((a, _) => a)
}

object ApplicativeInstances {
  import Applicative.ops._

  implicit val optionApplicative = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      (fa, fb) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a), Some(b)) => Some(f(a, b))
      }
  }

  implicit val listApplicative = new Applicative[List] {
    def pure[A](a: A): List[A] = a :: Nil
    def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
      (fa, fb) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (head :: tail, list2) =>
          map(list2)(x => f(head, x)) ++ map2(tail, list2)(f)
      }
  }

  import Id._
  implicit val idInstance = new Applicative[Id] {
    def pure[A](a:A):Id[A] = a
    def map2[A, B, C](id1: Id[A], id2: Id[B])(f: (A, B) => C): Id[C] =
      f(id1, id2)
  }
}