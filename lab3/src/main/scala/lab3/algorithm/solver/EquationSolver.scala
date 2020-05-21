package lab3.algorithm.solver

import cats.data.OptionT

trait EquationSolver[A, B, F[_]] {
  def findSolution(f: A => B, leftBound: A, rightBound: A): F[Option[(Int, B, B)]]
}