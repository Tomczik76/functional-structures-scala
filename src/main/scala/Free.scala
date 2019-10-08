trait Free[F[_], A] {
  import Free._

  def flatMap[B](fn: A => Free[F, B]): Free[F, B] = FlatMap(this, fn)

  def map[B](fn: A => B): Free[F, B] = flatMap(a => Pure(fn(a)))

  def foldMap[G[_]: Monad](fk: FunctionK[F, G]): G[A] = this match {
    case Lift(fa) => fk(fa)
    case Pure(a) => Monad[G].pure(a)
    case FlatMap(f: Free[F, e], fn) =>
      Monad[G].flatMap(f.foldMap(fk))((a:e) => fn(a).foldMap(fk))
  }
}

object Free {
  case class Pure[F[_], A](a:A) extends Free[F, A]

  def pure[F[_], A](a: A): Free[F, A] = Pure[F, A](a)

  case class Lift[F[_], A](fa: F[A]) extends Free[F, A]

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Lift(fa)

  case class FlatMap[F[_], A, B](f: Free[F, A], fn: A => Free[F, B]) extends Free[F, B]
}

object FreeMain {

  sealed trait Disk[A]
  case class Read(fileName:String) extends Disk[Array[Byte]]
  case class Write(fileName: String, data: Array[Byte]) extends Disk[Unit]
  case class Delete(fileName: String) extends Disk[Unit]

  val program = for {
    data <- Free.liftF(Read("data.txt"))
    _ <- Free.liftF(Write("data1.txt", data ++ "free".getBytes()))
    _ <- Free.liftF(Delete("data1.txt"))
  } yield ()

  val interpreter = new FunctionK[Disk, IO] {
    import java.io.{File, FileOutputStream}
    import java.nio.file.Files

    def apply[A](f:Disk[A]): IO[A] = f match {
      case Read(fileName) => IO(Files.readAllBytes(new File(fileName).toPath))
      case Write(fileName, bytes) => IO(new FileOutputStream(new File(fileName)).write(bytes))
      case Delete(fileName) => IO(Files.delete(new File(fileName).toPath))
    }
  }

  val compiledProgram = program.foldMap(interpreter)

  def main(args: Array[String]): Unit = {
    compiledProgram.run()
  }

}
