class Stack(T)
  # The items in the stack.
  @stack : Array(T)

  # Creates a new empty stack.
  def initialize
    @stack = Array(T).new
  end

  # Pushes the given *item* onto the stack.
  def push(item : T)
    @stack << item
  end

  # Remove the last item from the stack.
  def pop() : T
    @stack.pop
  end

  # Returns the number of items in the stack.
  def size
    @stack.size
  end

  # Returns the last item push onto the stack.
  def top : T
    @stack[-1]
  end

  # Returns `true` if the stack is empty, `false` otherwise.
  def empty? : Bool
    @stack.empty?
  end
end

def stack_example
  stack = Stack(Int32).new

  stack.push(1)
  stack.push(2)
  stack.push(3)

  puts "#{stack.size} items in the stack"

  puts "#{stack.pop} popped from the stack."
  puts "Top item in the stack: #{stack.top}"
  puts "#{stack.pop} popped from the stack."
  puts "#{stack.pop} popped from the stack."

  puts "The stack is empty" if stack.empty?
end

stack_example