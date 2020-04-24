object Euclid {

  def euclid_sub(a: Int, b: Int): Int =
    (Math.abs(a), Math.abs(b)) match {
      case (0, _) | (_, 0) => 0
      case (x, y) if x < y => euclid(x, y - x)
      case (x, y) if x > y => euclid(x - y, y)
      case _ => a
    }

  def euclid_mod(a: Int, b: Int): Int =
    (Math.abs(a), Math.abs(b)) match {
      case (_, 0) => a
      case (a, b) => euclid_mod(b, a % b)
    }

  def main(args: Array[String]): Unit = {
    println(euclid_sub(151 * 899, 151 * 182))
    println(euclid_mod(151 * 899, 151 * 182))
  }

}
