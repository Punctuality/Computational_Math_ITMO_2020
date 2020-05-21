package lab3.algorithm.solver

trait EquationSolver[A, B, F[_]] {
  def findSolution(f: A => B, leftBound: A, rightBound: A): F[Option[(Int, B, B)]]
}