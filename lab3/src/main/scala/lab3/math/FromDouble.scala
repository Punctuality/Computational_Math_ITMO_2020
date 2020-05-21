package lab3.math

trait FromDouble[B] {
  def fromDouble(double: Double): B
}

object FromDouble {
  object Implicits {
    implicit class fromDoubleOps(double: Double){
      def fromDoubleTo[B: FromDouble]: B = implicitly[FromDouble[B]].fromDouble(double)
    }
  }

  object Instances {
    implicit val double2byte: FromDouble[Byte] = _.toByte
    implicit val double2short: FromDouble[Short] = _.toShort
    implicit val double2char: FromDouble[Char] = _.toChar
    implicit val double2int: FromDouble[Int] = _.toInt
    implicit val double2long: FromDouble[Long] = _.toLong
    implicit val double2float: FromDouble[Float] = _.toFloat
    implicit val double2double: FromDouble[Double] = identity
  }
}