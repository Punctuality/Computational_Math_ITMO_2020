package lab4.algorithm

trait Interpolator[A, B, F[_]] {
  def interpolate(points: List[(A, B)]): F[Option[A => B]]
}
