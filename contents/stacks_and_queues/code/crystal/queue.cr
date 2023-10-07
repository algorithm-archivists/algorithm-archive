class Queue(T)
  # The items in the queue.
  @queue : Array(T)

  # Creates a new empty queue.
  def initialize
    @queue = Array(T).new
  end

  # Pushes the given *item* onto the queue and returns the size of the queue.
  def enqueue(item : T)
    @queue << item
    self.size
  end

  # Removes the first item in the queue (at index 0).
  def dequeue : T
    @queue.shift
  end

  # Returns the first item in the queue (at index 0).
  def front : T
    @queue[0]
  end

  # Returns the number of items in the queue.
  def size : Int32
    @queue.size
  end
end

def queue_example
  queue = Queue(Int32).new

  queue.enqueue(4)
  queue.enqueue(5)
  queue.enqueue(9)

  puts queue.dequeue
  puts queue.size
  puts queue.front
end

queue_example
