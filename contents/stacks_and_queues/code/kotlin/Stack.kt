class Stack<T> {
    private val stack = mutableListOf<T>()

    fun push(item: T) {
        stack.add(item)
    }

    fun pop(): T? {
        return if (stack.isEmpty()) {
            null
        } else stack.removeAt(stack.size - 1)
    }

    fun size(): Int = return stack.size

    fun top(): T? {
        return if (stack.isEmpty()) {
            null
        } else stack[stack.size - 1]
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