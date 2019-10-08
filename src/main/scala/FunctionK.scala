trait FunctionK[F[_], G[_]] {
  def apply[A](f:F[A]): G[A]
}

object Instances {
  val listToOption = new FunctionK[List, Option] {
    def apply[A](list:List[A]): Option[A] =
      list match {
        case Nil => None
        case head :: _ => Some(head)
      }
  }
}