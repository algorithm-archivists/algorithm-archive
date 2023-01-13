object Euclid {

  def euclid_sub(a: Int, b: Int): Int =
    (Math.abs(a), Math.abs(b)) match {
      case (0, _) | (_, 0) => 0
      case (x, y) if x < y => euclid_sub(x, y - x)
      case (x, y) if x > y => euclid_sub(x - y, y)
      case _ => a
    }

  def euclid_mod(a: Int, b: Int): Int =
    (Math.abs(a), Math.abs(b)) match {
      case (_, 0) => a
      case (a, b) => euclid_mod(b, a % b)
    }

  def main(args: Array[String]): Unit = {
    println("[#]\nModulus-based euclidean algorithm result:")
    println(euclid_mod(64 * 67, 64 * 81))
    println("[#]\nSubtraction-based euclidean algorithm result:")
    println(euclid_sub(128 * 12, 128 * 77))
  }

}
