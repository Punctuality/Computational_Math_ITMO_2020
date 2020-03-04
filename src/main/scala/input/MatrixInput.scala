package input

import math.Matrix

trait MatrixInput[A]{
  def produceMatrix: Option[Matrix[A]]
}
