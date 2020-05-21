package lab3.algorithm.separator

trait SolutionSeparator[A, B, F[_]] {
  def separateSolutions(f: A => B, leftBound: A, rightBound: A): F[List[((A, B), (A, B))]]
}