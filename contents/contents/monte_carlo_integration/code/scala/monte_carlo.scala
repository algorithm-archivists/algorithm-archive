object MonteCarlo {

  def inCircle(x: Double, y: Double) = x * x + y * y < 1

  def monteCarloPi(samples: Int) = {
    def randCoord = math.random() * 2 - 1

    var pointCount = 0

    for (_ <- 0 to samples)
      if (inCircle(randCoord, randCoord)) 
        pointCount += 1

    4.0 * pointCount / samples
  }

  def main(args: Array[String]): Unit = {
    val approxPi = monteCarloPi(1000)
    println("Estimated pi value: " + approxPi)
    println("Percent error: " + 100 * Math.abs(approxPi - Math.PI) / Math.PI)
  }
}
