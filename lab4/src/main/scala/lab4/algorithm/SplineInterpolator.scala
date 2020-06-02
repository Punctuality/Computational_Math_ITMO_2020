package lab4.algorithm

import cats.Monad
import cats.implicits._
import core.math.FromDouble
import core.math.FromDouble.Implicits._

import scala.math.Fractional
import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._
import scala.reflect.ClassTag

class SplineInterpolator[A: Fractional: FromDouble: ClassTag, F[_] : Monad] extends Interpolator[A, A, F]{

  private val cubicFunc: (A, A, A, A, A) => A => A = {
    case (x_i, a, b, c, d) =>
      ((x: A) => x - x_i).andThen(diff => a + b * diff + c * diff * diff + d * diff * diff * diff)
  }

  private def cubicSpline(splines: Seq[((A, A), A => A)]): A => A = {
    case x if x < splines.head._1._1 =>
      splines.head._2(x) // x is smaller than a (extrapolation)
    case x if x > splines.last._1._2 =>
      splines.last._2(x) // x is bigger than b  (extrapolation)
    case x => splines                                      // x is inside [a, b]  (interpolation)
      .find{ case ((xPrev, xNext), _) => (xPrev <= x) && (x <= xNext) }.get._2(x)
  }

  override def interpolate(points: List[(A, A)]): F[Option[A => A]] =
    Option(points).map(_.toArray).filter(_.length >= 3).pure[F].map{
      case Some(data: Array[(A, A)]) =>
        // Numerical Analysis R.L.Burden (Algorithm 3.4, p.168)
        val n = data.length - 1
        val (xs, ys): (Array[A], Array[A]) = data.unzip[A, A]
        val hs: Array[A] = Array.tabulate(n)(i => xs(i+1) - xs(i))
        val alphas: Array[A] = Range.inclusive(1, n-1)
            .map(i =>
              3.fromDoubleTo[A] * (ys(i+1) - ys(i)) / hs(i) -
                3.fromDoubleTo[A] * (ys(i) - ys(i-1)) / hs(i-1)
            ).toArray

        // tridiagonal linear system
        val l:  Array[A] = Array.fill(n+1)(1.fromDoubleTo[A])
        val mu: Array[A] = Array.fill(n)(0.fromDoubleTo[A])
        val z:  Array[A] = Array.fill(n+1)(0.fromDoubleTo[A])

        Range.inclusive(1, n - 1).foreach{ i =>
          l(i) = 2.fromDoubleTo[A] * (xs(i + 1) - xs(i - 1)) - hs(i - 1) * mu(i - 1)
          mu(i) = hs(i) / l(i)
          z(i) = (alphas(i-1) - hs(i - 1) * z(i - 1)) / l(i)
        }

        val as: Array[A] = ys
        val bs: Array[A] = Array.fill(n)(0.fromDoubleTo[A])
        val cs: Array[A] = Array.fill(n + 1)(0.fromDoubleTo[A])
        val ds: Array[A] = Array.fill(n)(0.fromDoubleTo[A])

        Range.inclusive(0, n - 1).reverse.foreach{ i =>
          cs(i) = z(i) - mu(i) * cs(i + 1)
          bs(i) = (as(i + 1) - as(i)) / hs(i) - hs(i) * (cs(i + 1) + 2.fromDoubleTo[A] * cs(i)) / 3.fromDoubleTo[A]
          ds(i) = (cs(i + 1) - cs(i)) / (3.fromDoubleTo[A] * hs(i))
        }

        val splines: Seq[((A, A), A => A)] =
          xs.indices.dropRight(1)
            .map(i => (xs(i), xs(i + 1)) -> cubicFunc(xs(i), as(i), bs(i), cs(i), ds(i)))
            .sortBy(_._1._1)

        Some apply cubicSpline(splines)
      case None => None
    }
}
