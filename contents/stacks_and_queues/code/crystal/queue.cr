class Queue(T)
  # The items in the queue.
  @queue : Array(T)

  # Creates a new empty queue.
  def initialize
    @queue = Array(T).new
  end

  # Pushes the given *item* onto the queue.
  def enqueue(item : T)
    @queue << item
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
  def size
    @queue.size
  end

  # Returns `true` if the queue is empty, `false` otherwise.
  def empty?
    @queue.empty?
  end
end

def queue_example
  queue = Queue(Int32).new

  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)

  puts "#{queue.size} items in the queue"

  puts "#{queue.dequeue} removed from the queue."
  puts "Front item in the queue: #{queue.front}"
  puts "#{queue.dequeue} removed from the queue."
  puts "#{queue.dequeue} removed from the queue."

  puts "The queue is empty" if queue.empty?
end

queue_example