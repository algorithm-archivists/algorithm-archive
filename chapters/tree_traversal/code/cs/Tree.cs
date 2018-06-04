// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace TreeTraversal
{
    public class Tree
    {
        private class Node
        {
            public List<Node> Children { get; set; } = new List<Node>();
            public int Id { get; set; }

            public Node(int id) => this.Id = id;
        }

        private Node root;

        public Tree(int depthCount, int childrenCount)
        {
            root = new Node(1);
            CreateAllChildren(root, depthCount, childrenCount);
        }

        public void DFSRecursive()
        {
            DFSRecursive(root);

            void DFSRecursive(Node node)
            {
                Console.WriteLine(node.Id);

                foreach (var c in node.Children)
                    DFSRecursive(c);
            }
        }

        public void DFSRecursivePostorder()
        {
            DFSRecursivePostorder(root);

            void DFSRecursivePostorder(Node node)
            {
                foreach (var c in node.Children)
                    DFSRecursivePostorder(c);

                Console.WriteLine(node.Id);
            }
        }

        public void DFSRecursiveInorderBinary()
        {
            DFSRecursiveInorderBinary(root);

            // This assumes only 2 children
            void DFSRecursiveInorderBinary(Node node)
            {
                if (node.Children.Count > 2)
                     throw new Exception("Not binary tree!");

                if (node.Children.Count > 0)
                {
                    DFSRecursiveInorderBinary(node.Children[0]);
                    Console.WriteLine(node.Id);
                    DFSRecursiveInorderBinary(node.Children[1]);
                }
                else
                    Console.WriteLine(node.Id);
            }
        }

        public void DFSStack()
        {
            var stack = new Stack<Node>();
            stack.Push(root);
            Node temp;

            while (stack.Count != 0)
            {
                Console.WriteLine(stack.Peek().Id);
                temp = stack.Pop();

                foreach (var c in temp.Children)
                    stack.Push(c);
            }
        }

        public void BFSQueue()
        {
            var queue = new Queue<Node>();
            queue.Enqueue(root);
            Node temp;

            while (queue.Count != 0)
            {
                Console.WriteLine(queue.Peek().Id);
                temp = queue.Dequeue();

                foreach (var c in temp.Children)
                    queue.Enqueue(c);
            }
        }

        private void CreateAllChildren(Node node, int rowCount, int childrenCount)
        {
            if (rowCount <= 1)
                return;

            for (int i = 0; i < childrenCount; i++)
            {
                node.Children.Add(new Node(node.Id * 10 + i + 1));
                CreateAllChildren(node.Children[i], rowCount - 1, childrenCount);
            }
        }
    }
}
