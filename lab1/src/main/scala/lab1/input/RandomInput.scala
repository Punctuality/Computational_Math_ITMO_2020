package lab1.input

import lab1.math.{Matrix, MatrixDense}

import scala.util.Random

class RandomInput[A](rndFunc: () => A, rows: Int, cols: Int) extends MatrixInput[A] {
  override def produceMatrix: Option[Matrix[A]] = Some apply MatrixDense.matrixFromFunction((_, _) => rndFunc(), rows, cols)
}

object RandomInput{
  def doubleRandom (rows: Int, cols: Int):             RandomInput[Double]  = new RandomInput(Random.nextDouble, rows, cols)
  def longRandom   (rows: Int, cols: Int):             RandomInput[Long]    = new RandomInput(Random.nextLong, rows, cols)
  def intRandom    (rows: Int, cols: Int):             RandomInput[Int]     = new RandomInput(Random.nextInt, rows, cols)
  def intRandom    (rows: Int, cols: Int, bound: Int): RandomInput[Int]     = new RandomInput(() => Random.nextInt(bound), rows, cols)
  def booleanRandom(rows: Int, cols: Int):             RandomInput[Boolean] = new RandomInput(Random.nextBoolean, rows, cols)
}
