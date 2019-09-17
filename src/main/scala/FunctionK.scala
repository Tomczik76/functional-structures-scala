trait FunctionK[F[_], G[_]] {
  def apply[A](f:F[A]): G[A]
}
