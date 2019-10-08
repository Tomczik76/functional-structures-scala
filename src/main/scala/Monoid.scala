import simulacrum._

@typeclass trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object MonoidInstances {
  import Semigroup.ops._

  implicit val intInstance = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }
  assert(3 == (1 |+| 2 |+| 3))
  implicit val stringInstance = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y:String):String = x + y
  }
  assert("abcedf" == ("ab" |+| "cd" |+| "ef"))

  implicit def listInstance[A] = new Monoid[List[A]] {
    def empty: List[A] = Nil
    def combine(x:List[A], y:List[A]) = x ++ y
  }
  assert(List(1, 2, 3) == (List(1) |+| List(2, 3)))


  implicit def tuple2Instance[A: Monoid, B: Monoid] = new Monoid[Tuple2[A, B]] {
    val empty:(A, B) = (Monoid[A].empty, Monoid[B].empty)

   def combine(x:(A, B), y:(A, B)): (A, B) =
      (x._1 |+| y._1,x._2 |+| y._2)
  }
  assert((3, "abcd") == ((1, "ab") |+| (2, "cd")))

  implicit def tuple3Instance[A: Monoid, B: Monoid, C: Monoid] = new Monoid[Tuple3[A, B, C]] {
    val empty:(A, B, C) = (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty)

   def combine(x:(A, B, C), y:(A, B, C)): (A, B, C) =
      (x._1 |+| y._1,x._2 |+| y._2, x._3 |+| y._3)
  }

  implicit def mapInstance[A, B: Monoid] = new Monoid[Map[A, B]] {
    val empty: Map[A, B]  = Map.empty[A, B]
    def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] = {
      (x.toSeq ++ y.toSeq).foldLeft(empty){
        case (map, (key, value)) =>
          val accumValue =  map.get(key).getOrElse(Monoid[B].empty)
          map + (key -> (accumValue |+| value))
      }
    }
  }
  assert(Map("foo" -> 1, "bar" -> 2) == (Map("foo" -> 1, "bar" -> 1) |+| Map("bar" -> 1)))
}

object OthelloWordCount {
  def toLowerCaseWords(text:String): Seq[String] = text.toLowerCase.split("[^a-zA-Z']+")

  def toMonoid(word: String): (Int, Int, Map[String, Int]) = (1, word.length, Map(word -> 1))

  def resultToMessage(tuple: (Int, Int, Map[String, Int])): String = tuple match {
    case (wordCount, letters,  words) =>
      val characters = Set("brabantio", "gratiano", "lodovico", "othello",  "cassio",  "iago", "roderigo", "montano", "clown", "desdemona", "emilia", "bianca", "herald", "moor")

      s""""Word Count: $wordCount
          |Average Word Length: ${letters / wordCount}
          |Unique Word Count: ${words.size}
          |Characters: ${characters.map(c => (c, words.get(c))).mkString(", ")}""".stripMargin
  }
}

object Main {
  //import MonoidInstances._
  //import Semigroup.ops._
  import cats._
  import cats.implicits._
  import OthelloWordCount._

  def foldMap[A, B: cats.Monoid](as: Seq[A], fn: A => B): B = {
    as.foldLeft(cats.Monoid[B].empty)((b, a) => b |+| fn(a))
  }
  Nil
  def main(args: Array[String]) = {
    val result =
      io.Source.fromFile("2600-0.txt").getLines
      .toStream
      .flatMap(toLowerCaseWords)
      .foldMap(toMonoid)

   // val result = foldMap(words, toMonoid)

    println(resultToMessage(result))

  }
}


import cats.effect._
object MainFs2 extends IOApp {
  import cats._
  import cats.implicits._

  import fs2._
  import fs2.io.file.readAll
  import cats.effect.implicits._
  import java.nio.file._
  import scala.concurrent.ExecutionContext.global
  import OthelloWordCount._

  def getFileLines(path:Path) =
    readAll[cats.effect.IO](path, Blocker.liftExecutionContext(global), 4096)
      .through(text.utf8Decode)
      .through(text.lines)

  def run(args: List[String]):cats.effect.IO[ExitCode] = {

    val parallelism = Runtime.getRuntime().availableProcessors()

   readAll[cats.effect.IO](Paths.get("2600-0.txt"), Blocker.liftExecutionContext(global), 4096)
     .through(text.utf8Decode)
     .through(text.lines)
     .map(line => Stream.emits(toLowerCaseWords(line)).map(toMonoid).covary[cats.effect.IO])
     .parJoin(parallelism)
     .compile
     .foldMonoid
     .map(resultToMessage(_))
     .flatMap(msg => cats.effect.IO(println(msg)))
     .as(ExitCode.Success)
    
  }
}


