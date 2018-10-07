fun bubbleSort(input: MutableList<Int>) {
    for (i in (input.size - 1) downTo 0) {
        for (j in 0 until i) {
            if (input[j] > input[j + 1]) {
                input[j] = input[j + 1].also {
                    input[j + 1] = input[j]
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    var list = mutableListOf(4, 2, 9, 20, 11, 30, 1, 0);
    println("Original $list")
    bubbleSort(list)
    println("Sorted   $list")
}
