interface IStack<T> {
  /**
   * `pop` removes last element from the stack and returns the same
   */
  pop(): T;
  /**
   * `push` adds element to last of the stack and returns the size
   */
  push(data: T): number;
  /**
   * `size` return size or length of the stack
   */
  size(): number;
  /**
   * `top` returns last element of the stack
   */
  top(): T;
}

class Stack<T> implements IStack<T> {
  private readonly list: Array<T> = [];

  public push(data: T) {
    return this.list.push(data);
  }

  public pop() {
    return this.list.pop();
  }

  public size() {
    return this.list.length;
  }

  public top() {
    return this.list[this.list.length - 1];
  }
}

function exampleStack() {
  const numberStack = new Stack<number>();

  numberStack.push(4);
  numberStack.push(5);
  numberStack.push(9);

  console.log(numberStack.pop());
  console.log(numberStack.size());
  console.log(numberStack.top());
}

exampleStack();
