package lab3.algorithm.multisolver

trait MultiEquationSolver[A, F[_]] {
  def findSolution(fs: List[A] => List[A], x: List[A]): F[Option[(Int, List[A])]]
}
