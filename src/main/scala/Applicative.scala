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

  implicit def optionApplicative = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      (fa, fb) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a), Some(b)) => Some(f(a, b))
      }
  }


  assert(Some(1) == (Option(1) <* Option(2)))
  assert(Some(2) == (Option(1) *> Option(2)))
  assert(None == (Option(1) <* None))

  assert(Some("a", 1) == (Option("a").product(Option(1))))
  assert(None == (Option("a").product(None)))

  val add: Int => Int => Int  =
     a => b => a + b

  assert(Some(3) == (Option(add) <*> Option(1) <*> Option(2)))   

  assert(None == (Option(add) <*> Option(1) <*> None))

  def addApplicatives[F[_]: Applicative](f1: F[Int], f2: F[Int]): F[Int] =
   Applicative[F].map2(f1, f2)((a, b) => a + b)

  

  implicit def listApplicative = new Applicative[List] {
    def pure[A](a: A): List[A] = a :: Nil
    def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
      (fa, fb) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (head :: tail, list2) =>
          map(list2)(x => f(head, x)) ++ map2(tail, list2)(f)
      }
  }
}