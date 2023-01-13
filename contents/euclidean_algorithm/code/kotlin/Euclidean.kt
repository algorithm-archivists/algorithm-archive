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

fun main(args: Array<String>) {
    println("[#]\nModulus-based euclidean algorithm result:")
    println(euclidMod(64 * 67, 64 * 81))
    println("[#]\nSubtraction-based euclidean algorithm result:")
    println(euclidSub(128 * 12, 128 * 77))
}