using System;
using System.Collections.Generic;

namespace my
{
    /**
     * implementation using linked list
     * [value][next] -> [value][next] -> ... -> [value][next]
     * (top Node)      (intermediat Nodes)
     * left most Node represents top element of stack
     */
    public class Node {
        public Node next;
        public object data;
    }

    public class Stack {
        private Node head;
        public int size;

        public Stack() {
            head = new Node();
            size = 0;
        }
        public void Push(object value) {
            Node newNode = new Node();
            newNode.data = value;
            newNode.next = head.next;
            head.next = newNode;
            size++;
        }

        public void Pop() {
            if (size != 0) {
                head.next = head.next.next;
                size--;
            }
            else {
                Console.WriteLine("No element to remove.");
            }
        }

        public object Top() {
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

            Console.WriteLine("Stack:");
            Stack intStack = new Stack();

            intStack.Push(4);
            intStack.Push(5);
            intStack.Push(9);

            Console.Write("Size: ");
            Console.WriteLine(intStack.Size());
            Console.Write("Top: ");
            Console.WriteLine(intStack.Top());

            intStack.Pop();

            Console.Write("Size: ");
            Console.WriteLine(intStack.Size());
            Console.Write("Top: ");
            Console.WriteLine(intStack.Top());
        }
    }
}
