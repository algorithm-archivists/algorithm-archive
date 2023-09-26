class Queue<T> {
    private val list = mutableListOf<T>()

    fun enqueue(item: T) {
        list.add(item)
    }

    fun dequeue(): T? {
        return if (list.isEmpty()) {
            null
        } else list.removeAt(0)
    }

    fun front(): T? {
        return if (list.isEmpty()) {
            null
        } else list[0]
    }

    fun size(): Int = return list.size
}

fun main(args: Array<String>) {
    val queue = Queue<Int>()
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)

    println("Front: ${queue.front()}")
    println(queue.dequeue())
    println("Size: ${queue.size()}")
    println(queue.dequeue())
}