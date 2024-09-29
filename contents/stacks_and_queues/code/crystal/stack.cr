class Stack(T)
  # The items in the stack.
  @stack : Array(T)

  # Creates a new empty stack.
  def initialize
    @stack = Array(T).new
  end

  # Pushes the given *item* onto the stack and returns the size of the stack.
  def push(item : T)
    @stack << item
    self.size
  end

  # Remove the last item from the stack.
  def pop() : T
    @stack.pop
  end

  # Returns the number of items in the stack.
  def size : Int32
    @stack.size
  end

  # Returns the last item push onto the stack.
  def top : T
    @stack[-1]
  end
end

def stack_example
  stack = Stack(Int32).new

  stack.push(4)
  stack.push(5)
  stack.push(9)

  puts stack.pop
  puts stack.size
  puts stack.top
end

stack_example
