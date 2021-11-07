interface IQueue<T> {
  /**
   * `dequeue` removes first element from the queue and returns the same
   */
  dequeue(): T;
  /**
   * `enqueue` adds element to last of the queue and returns the size
   */
  enqueue(data: T): number;
  /**
   * `size` return size or length of the queue
   */
  size(): number;
  /**
   * `front` returns first element of the queue
   */
  front(): T;
}

class Queue<T> implements IQueue<T> {
  private readonly list: Array<T> = [];

  public enqueue(data: T) {
    return this.list.push(data);
  }

  public dequeue() {
    return this.list.shift();
  }

  public size() {
    return this.list.length;
  }

  public front() {
    return this.list[0];
  }
}

function exampleQueue() {
  const numberQueue = new Queue<number>();

  numberQueue.enqueue(4);
  numberQueue.enqueue(5);
  numberQueue.enqueue(9);

  console.log(numberQueue.dequeue());
  console.log(numberQueue.size());
  console.log(numberQueue.front());
}

exampleQueue();
