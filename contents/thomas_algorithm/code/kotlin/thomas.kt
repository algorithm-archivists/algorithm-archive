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

    System.out.format("The system,\n")
    System.out.format("[%.1f, %.1f, %.1f][x] = [%.1f]\n", b[0], c[0], 0f, x[0])
    System.out.format("[%.1f, %.1f, %.1f][y] = [%.1f]\n", a[1], b[1], c[1], x[1])
    System.out.format("[%.1f, %.1f, %.1f][z] = [%.1f]\n", 0f, a[2], b[2], x[2])
    System.out.format("has the solution:\n")

    for (i in solution.indices) {
        System.out.format("[% .5f]\n", solution[i])
    }
}
