import java.util.Random

private fun inCircle(x: Double, y: Double, radius: Double = 1.0) = (x * x + y * y) < radius * radius

fun monteCarlo(samples: Int): Double {
    var piCount = 0
    val random = Random()

    for (i in 0 until samples) {
        val x = random.nextDouble()
        val y = random.nextDouble()
        if (inCircle(x, y))
            piCount++
    }
    return 4.0 * piCount / samples
}

fun main(args: Array<String>) {
    val piEstimate = monteCarlo(100000)
    println("Estimated pi value: $piEstimate")
    val percentError = 100 * Math.abs(piEstimate - Math.PI) / Math.PI
    println("Percent error: $percentError")
}
