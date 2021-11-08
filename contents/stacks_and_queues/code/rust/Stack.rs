struct Stack<T> {
	list: Vec<T>
}

impl<T> Stack<T> {
    fn new() -> Self {
        Stack {
            list: Vec::new(),
        }
    }

    // Note that this returns a reference to the value
    // This is in contrast to pop which gives ownership of the value
    fn top(&self) -> Option<&T> {
        if self.list.len() > 0 {
            Some(&self.list[self.list.len() - 1])
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<T> {
        self.list.pop()
    }

    fn push(&mut self, item: T) {
        self.list.push(item);
    }

    fn size(&self) -> usize {
        self.list.len()
    }
}

fn main() {
    let mut i32stack: Stack<i32> = Stack::new();

    i32stack.push(4);
    i32stack.push(5);
    i32stack.push(6);

    println!("{:?}", i32stack.pop().unwrap());
    println!("{:?}", i32stack.size());
    println!("{:?}", i32stack.top().unwrap());
}
