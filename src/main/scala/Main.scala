trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]

  object Ops {
    implicit class semigroupOps[A:Semigroup](m: A) {
      def combine(that: A): A = Semigroup[A].combine(m, that)
      def |+|(that: A): A = Semigroup[A].combine(m, that)
    }
  }
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

  implicit val intInstance = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }

  implicit val stringInstance = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y:String):String = x + y
  }

  implicit def listInstance[A] = new Monoid[List[A]] {
    def empty: List[A] = Nil
    def combine(x:List[A], y:List[A]) = x ++ y
  }

  implicit def tuple2Instance[A: Monoid, B: Monoid] = new Monoid[Tuple2[A, B]] {
    val empty:(A, B) = (Monoid[A].empty, Monoid[B].empty)
    def combine(x:(A, B), y:(A, B)): (A, B) =
      (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
  }

  implicit def mapInstance[A, B: Monoid] = new Monoid[Map[A, B]] {
    val empty: Map[A, B]  = Map.empty[A, B]
    def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] = {
      (x.toSeq ++ y.toSeq).foldLeft(Map.empty[A, B]){ case (map, (key, value)) =>
        map + (key -> Monoid[B].combine(value, map.get(key).getOrElse(Monoid[B].empty)))
      }
    }
  }
}

object Main {
  import Semigroup.Ops._

  def foldMap[A, B: Monoid](as: Seq[A])(fn: A => B): B = {
    as.foldLeft(Monoid[B].empty)((b, a) => b |+| fn(a))
  }

  def main(args: Array[String]) = {
    println(foldMap(text.split("[^a-zA-Z']+"))(str => (1, Map((str.toLowerCase, 1)))))
  }


  val text = "Category theory formalizes mathematical structure and its concepts in terms of a labeled directed graph called a category, whose nodes are called objects, and whose labelled directed edges are called arrows (or morphisms). A category has two basic properties: the ability to compose the arrows associatively, and the existence of an identity arrow for each object. The language of category theory has been used to formalize concepts of other high-level abstractions such as sets, rings, and groups. Informally, category theory is a general theory of functions."
}
