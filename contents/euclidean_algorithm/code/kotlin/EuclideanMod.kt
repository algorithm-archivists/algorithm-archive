import kotlin.math.absoluteValue

fun euclidMod(a: Int, b: Int): Int {
    var a = a.absoluteValue
    var b = b.absoluteValue

    while (b != 0) {
        val tmp = b
        b = a % b
        a = tmp
    }

    return a
}

fun main() {
    println(euclidMod(64 * 67, 64 * 81))
}