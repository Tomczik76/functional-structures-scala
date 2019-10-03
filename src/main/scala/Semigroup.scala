import simulacrum._

@typeclass trait Semigroup[A] {
  @op("|+|") def combine(x: A, y: A): A
}