object Euclid {

  def euclid(a: Int, b: Int):Int =
    (a,b) match{
      case (x, y) if x < y => euclid(x, y-x)
      case (x, y) if x > y => euclid(x-y, y)
      case _ => a
    }

  def euclid_mod(a: Int, b: Int):Int =
    b match{
      case 0 => a
      case _ => euclid_mod(b, a % b)
    }

  def main(args: Array[String]): Unit =
    println(euclid(135749,27482))
    println(euclid_mod(135749,27482))

}