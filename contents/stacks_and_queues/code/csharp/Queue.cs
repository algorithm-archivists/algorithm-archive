using System;
using System.Collections.Generic;

namespace my {
    /**
     * implementation using linked list
     * [value][next] -> [value][next] -> ... -> [value][next]
     * (top Node)      (intermediat Nodes)
     * left most Node represents top element of queue
     */
    public class Node {
        public Node next;
        public object data;
    }
    public class Queue {
        private Node head;
        private Node tail;
        public int size;

        public Queue() {
            head = new Node();
            tail = head;
            size = 0;
        }
        public void Enqueue(object value) {
            Node newNode = new Node();
            newNode.data = value;
            tail.next = newNode;
            tail = newNode;
            size++;
        }

        public void Dequeue() {
            if (size != 0) {
                head.next = head.next.next;
                size--;
            }
            else {
                Console.WriteLine("No element to remove.");
            }
        }

        public object Front() {
            return head.next.data;
        }

        public int Size() {
            return size;
        }

        public bool Empty() {
            return size == 0;
        }

    }

    class Test {
        static void Main(string[] args) {
            Console.WriteLine("Queue:");
            Queue intQueue = new Queue();

            intQueue.Enqueue(4);
            intQueue.Enqueue(5);
            intQueue.Enqueue(9);

            Console.Write("Size: ");
            Console.WriteLine(intQueue.Size());
            Console.Write("Front: ");
            Console.WriteLine(intQueue.Front());

            intQueue.Dequeue();

            Console.Write("Size: ");
            Console.WriteLine(intQueue.Size());
            Console.Write("Front: ");
            Console.WriteLine(intQueue.Front());

        }
    }
}
