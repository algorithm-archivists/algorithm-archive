use std::collections::VecDeque;

struct Queue<T> {
    list: VecDeque<T>
}

impl<T> Queue<T> {
    fn new() -> Self {
       Queue{
           list: VecDeque::new(),
       }
    }

    // Note that this returns a reference to the value
    // This is in contrast to dequeue which gives ownership of the value
    fn front(&self) -> Option<&T> {
        self.list.front()
    }

    fn dequeue(&mut self) -> Option<T> {
        self.list.pop_front()
    }

    fn enqueue(&mut self, item: T) {
        self.list.push_back(item);
    }

    fn size(&self) -> usize {
        self.list.len()
    }
}

fn main() {
    let mut i32queue = Queue::new();

    i32queue.enqueue(4);
    i32queue.enqueue(5);
    i32queue.enqueue(6);

    println!("{:?}", i32queue.dequeue().unwrap()); // 4
    println!("{:?}", i32queue.size()); // 2
    println!("{:?}", i32queue.front().unwrap()); // 5
}
