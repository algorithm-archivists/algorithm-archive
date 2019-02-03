import kotlin.math.absoluteValue

fun euclidSub(a: Int, b: Int): Int {
    var a = a.absoluteValue
    var b = b.absoluteValue

    while (a != b) {
        if (a > b) a -= b
        else b -= a
    }

    return a
}

fun main() {
    println(euclidSub(128 * 12, 128 * 77))
}
