abstract class IO[A] {
  val run: () =>  A
}

case class Delay[A] (run: () => A) extends IO[A]

object IO {

  def apply[A](thunk: => A): IO[A] = Delay(() => thunk)

  implicit val ioMonad = new Monad[IO] {
    override def pure[A](a: A): IO[A] = apply(a)
    override def map[A, B](fa: IO[A])(fn: A => B): IO[B] =
      IO(fn(fa.run()))

    override def flatten[A](ffa: IO[IO[A]]): IO[A] =
    ffa.run()

    def flatMap[A, B](fa: IO[A], fn: A => IO[B]): IO[B] =
      IO(fn(fa.run()).run())
  }
}
