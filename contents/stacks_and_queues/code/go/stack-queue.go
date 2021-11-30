package main

import (
	"fmt"
	"os"
)

type stackNode struct {
	value interface{}
	prev  *stackNode
}

type Stack struct {
	top    *stackNode
	length int
}

// NewStack initializes a new stack.
func NewStack() *Stack {
	return &Stack{top: nil, length: 0}
}

// Push adds an element to the stack and returns the length of the stack.
func (s *Stack) Push(v interface{}) int {
	n := &stackNode{value: v, prev: s.top}
	s.top = n
	s.length++
	return s.length
}

// Pop removes and returns the last item from the stack.
func (s *Stack) Pop() interface{} {
	if s.length == 0 {
		fmt.Fprint(os.Stderr, "cannot pop items of an empty stack")
		os.Exit(1)
	}
	item := s.top
	s.top = item.prev
	s.length--
	return item.value
}

// Size returns the size of the stack.
func (s Stack) Size() int {
	return s.length
}

// Top returns the last item in the stack.
func (s Stack) Top() interface{} {
	if s.length == 0 {
		fmt.Fprint(os.Stderr, "cannot peek an empty stack")
		os.Exit(1)
	}
	return s.top.value
}

type queueNode struct {
	value interface{}
	next  *queueNode
}

type Queue struct {
	front  *queueNode
	length int
}

// NewQueue initializes a new queue.
func NewQueue() *Queue {
	return &Queue{front: nil, length: 0}
}

// Enqueue adds an item to the queue and returns the length of the queue.
func (q *Queue) Enqueue(v interface{}) int {
	n := &queueNode{value: v, next: nil}
	if q.front == nil {
		q.front = n
	} else {
		last := q.front
		for last.next != nil {
			last = last.next
		}
		last.next = n
	}
	q.length++
	return q.length
}

// Dequeue removes and returns an item from the queue.
func (q *Queue) Dequeue() interface{} {
	if q.length == 0 {
		fmt.Fprint(os.Stderr, "cannot dequeue an empty queue")
		os.Exit(1)
	}
	item := q.front
	if q.length == 1 {
		q.front = nil
	} else {
		q.front = q.front.next
	}
	q.length--
	return item.value
}

// Size returns the size of the queue.
func (q Queue) Size() int {
	return q.length
}

// Front returns the first element in the queue.
func (q Queue) Front() interface{} {
	if q.length == 0 {
		fmt.Fprint(os.Stderr, "cannot peek an empty queue")
		os.Exit(1)
	}
	return q.front.value
}

func main() {
	stack := NewStack()

	stack.Push(4)
	stack.Push(5)
	stack.Push(9)

	fmt.Println(stack.Pop())
	fmt.Println(stack.Size())
	fmt.Println(stack.Top())

	queue := NewQueue()

	queue.Enqueue(4)
	queue.Enqueue(5)
	queue.Enqueue(9)

	fmt.Println(queue.Dequeue())
	fmt.Println(queue.Size())
	fmt.Println(queue.Front())
}
