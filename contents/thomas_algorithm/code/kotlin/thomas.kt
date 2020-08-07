private fun thomas(a: DoubleArray, b: DoubleArray, c: DoubleArray, d: DoubleArray): DoubleArray {
    val cPrime = c.clone()
    val x = d.clone()
    val size = a.size
    cPrime[0] /= b[0]
    x[0] /= b[0]
    for (i in 1 until size) {
        val scale = 1.0 / (b[i] - cPrime[i - 1] * a[i])
        cPrime[i] *= scale
        x[i] = (x[i] - a[i] * x[i - 1]) * scale
    }
    for (i in (size - 2) downTo 0) {
        x[i] -= cPrime[i] * x[i + 1]
    }
    return x
}

fun main(args: Array<String>) {
    val a = doubleArrayOf(0.0, 2.0, 3.0)
    val b = doubleArrayOf(1.0, 3.0, 6.0)
    val c = doubleArrayOf(4.0, 5.0, 0.0)
    val x = doubleArrayOf(7.0, 5.0, 3.0)
    val solution = thomas(a, b, c, x)

    println("System:")
    println("[%.1f, %.1f, %.1f][x] = [%.1f]".format(b[0], c[0], 0f, x[0]))
    println("[%.1f, %.1f, %.1f][y] = [%.1f]".format(a[1], b[1], c[1], x[1]))
    println("[%.1f, %.1f, %.1f][z] = [%.1f]\n".format(0f, a[2], b[2], x[2]))
    println("Solution:")
    for (i in solution.indices) {
        println("[% .5f]".format(solution[i]))
    }
}
