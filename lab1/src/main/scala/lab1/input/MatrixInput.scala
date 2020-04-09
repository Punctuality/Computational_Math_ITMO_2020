package lab1.input

import lab1.math.Matrix

trait MatrixInput[A]{
  def produceMatrix: Option[Matrix[A]]
}
