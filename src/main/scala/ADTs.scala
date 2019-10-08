object ADTs {


  def product[A, B](a: A, b: B): (A, B) = (a, b)
  def first[A, B](x: (A, B)): A = x._1
  def second[A, B](x: (A, B)): B = x._2

  // 1 + a
  sealed trait Option[+A]
  case object None extends Option[Nothing]
  case class Some[A](a:A) extends Option[A]

  // a + b
  sealed trait Either[+A, +B]
  case class Left[A, B](a:A) extends Either[A, B]
  case class Right[A, B](b:B) extends Either[A, B]

  def left[A, B](a: A): Either[A, B] = Left(a)
  def right[A, B](b: B): Either[A, B] = Right(b)

  sealed trait List[+A]
  case class Cons[A](head:A, tail:List[A]) extends List[A]
  case object Nil extends List[Nothing]

  sealed trait Tree[+A]
  case class Node[A](left:Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  // a + (b + c) = (a + b) + c
  def additveAssocativity[A, B, C](in: Either[A, Either[B, C]]): Either[Either[A, B], C] =
    in match {
      case Left(a) => Left(Left(a))
      case Right(Left(b)) => Left(Right(b))
      case Right(Right(c)) => Right(c)
    }

  def additiveAssociativity[A, B, C](in: Either[Either[A, B], C]): Either[A, Either[B, C]] =
    in match {
      case Left(Left(a)) => Left(a)
      case Left(Right(b)) => Right(Left(b))
      case Right(c) => Right(Right(c))
    }

  // a * (b * c) = (a * b) * c
  def multipicativeAssociativity[A, B, C](in: (A, (B, C))): ((A, B), C) =
    in match {
      case (a, (b, c)) => ((a, b), c)
    }

  def multipicativeAssociativity2[A, B, C](in: ((A, B), C)): (A, (B, C)) =
    in match {
      case ((a, b), c) => (a, (b, c))
    }

  // 1 * a = a
  def multiplicativeIdentity[A](a:A): (Unit, A) = ((), a)
  def multiplicativeIdentity[A](tup: (Unit, A)): A = tup._2

  // 0 * a = 0
  def multipicativeAbsorb[A](nothing: Nothing): (Nothing, A) = ???
  def multipicativeAbsorb[A](tup: (Nothing, A)): Nothing = ???

  // 0 + a = a
  def additiveIdentity[A](either: Either[Nothing, A]): A = either match {
    case Left(nothing) => ???
    case Right(a) => a
  }
  def additiveIdentity[A](a:A): Either[Nothing, A] = Right(a)

  // a * (b + c) = a * b + a * c
  def distributive[A, B, C](tup: (A, Either[B, C])): Either[(A, B), (A, C)] =
    tup match {
      case (a, Left(b)) => Left((a, b))
      case (a, Right(c)) => Right((a, c))
    }

  def distributive[A, B, C](either: Either[(A, B), (A, C)]): (A, Either[B, C]) =
    either match {
      case Left((a, b)) => (a, Left(b))
      case Right((a, c)) => (a, Right(c))
    }

  // X^A * X^B = X^(A + B)
  def exponent[X, A, B](tup: (A => X, B => X)): Either[A, B] => X = {
   val (fn1, fn2) = tup
   (either:Either[A, B]) => either match {
      case Left(a) => fn1(a)
      case Right(b) => fn2(b)
   }
  }

  def exponent[X, A, B](fn: Either[A, B] => X): (A => X, B => X) =
    ((a:A) => fn(Left(a)), (b:B) => fn(Right(b)))
   

  // A^2 = A * A
  def square[A](fn: A => A, a: A): (A, A) = (a, fn(a))
  def square[A](a: A): A => A = (_:A) => a

  // X^1 = X
  def exponetialIdentity[A](a: A): Unit => A = (unit:Unit) => a
  def exponetialIdentity[A](fn: Unit => A): A = fn(())


  // 1^a == 1
  def fn11[A](fn: A => Unit, a: A): Unit = ()
  def fn12[A]: A => Unit = a => ()

  // A^B * A^C == A^(B + C)
  def fn13[A, B, C](tup: (B => A, C => A)): Either[B, C] => A = {
    val (fn1, fn2) = tup
    (either: Either[B, C]) => {
      either match {
        case Left(b) => fn1(b)
        case Right(c) => fn2(c)
      }
    }
  }

  def fn14[A, B, C](fn: Either[B, C] => A): (B => A, C => A) =
    ((b) => fn(Left(b)), (c) => fn(Right(c)))

}
