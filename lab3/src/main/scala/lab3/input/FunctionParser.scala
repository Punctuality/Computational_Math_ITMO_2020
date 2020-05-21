package lab3.input

import cats.Applicative
import cats.syntax.applicative._
import com.github.blemale.scaffeine.{LoadingCache, Scaffeine}
import lab3.math.FromDouble
import lab3.math.FromDouble.Implicits._
import org.nfunk.jep.JEP

import scala.math.Numeric.Implicits._

trait FunctionParser {

  private val parsersCache: LoadingCache[String, JEP] = Scaffeine().build(_ => {
    val tmpP = new JEP()
    tmpP.setImplicitMul(true)
    tmpP.addStandardConstants()
    tmpP.addStandardFunctions()
    tmpP
    }
  )

  def parserExpression[A: Numeric, B: FromDouble, F[_]: Applicative](expression: String)(values: Map[String, A]): F[B] = {
    val parser = parsersCache.get(expression)
    parser.getSymbolTable.clearNonConstants()
    values.toList.foreach{ case (varName, varVal) => parser.addVariable(varName, varVal.toDouble) }
    parser.parseExpression(expression)
    parser.getValue.fromDoubleTo[B]
  }.pure[F]

  def parseExpressionByX[A: Numeric, B: FromDouble, F[_]: Applicative](expression: String): A => F[B] =
    aVal => parserExpression(expression)(Map("x" -> aVal))
}
