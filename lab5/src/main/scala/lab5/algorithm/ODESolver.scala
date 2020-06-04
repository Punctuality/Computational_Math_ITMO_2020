package lab5.algorithm

import lab5.math.OrdinaryDifferentialEquation

trait ODESolver[A, F[_], G[_]] {
  def solveODE(ode: OrdinaryDifferentialEquation[A, G], startPoint: (A, A), step: A, count: Int): F[List[(A, A)]]
}
