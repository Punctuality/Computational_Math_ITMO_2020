package core.input

import cats.Applicative
import cats.syntax.applicative._
import com.github.blemale.scaffeine.{LoadingCache, Scaffeine}
import core.math.FromDouble
import core.math.FromDouble.Implicits._
import org.nfunk.jep.JEP

import scala.Numeric.Implicits._

trait FunctionParser {

  private val parsersCache: LoadingCache[String, JEP] = Scaffeine().build(_ => {
    val tmpP = new JEP()
    tmpP.setImplicitMul(true)
    tmpP.addStandardConstants()
    tmpP.addStandardFunctions()
    tmpP
    }
  )

  def parseExpression[A: Numeric, B: FromDouble, F[_]: Applicative](expression: String)(values: Map[String, A]): F[B] = {
    val parser = parsersCache.get(expression)
    parser.getSymbolTable.clearNonConstants()
    values.toList.foreach{ case (varName, varVal) => parser.addVariable(varName, varVal.toDouble) }
    parser.parseExpression(expression)
    parser.getValue.fromDoubleTo[B]
  }.pure[F]

  def parseExpressionByX[A: Numeric, B: FromDouble, F[_]: Applicative](expression: String): A => F[B] =
    aVal => parseExpression(expression)(Map("x" -> aVal))
}
