class Stack<T> {
    private val list = mutableListOf<T>()

    fun push(item: T) {
        list.add(item)
    }

    fun pop(): T? {
        return if (list.isEmpty()) {
            null
        } else list.removeAt(list.size - 1)
    }

    fun size(): Int = list.size

    fun top(): T? {
        return if (list.isEmpty()) {
            null
        } else list[list.size - 1]
    }
}

fun main(args: Array<String>) {
    val stack = Stack<Int>()
    stack.push(1)
    stack.push(2)
    stack.push(3)

    println("Top: ${stack.top()}")
    println(stack.pop())
    println("Size: ${stack.size()}")
    println(stack.pop())
}