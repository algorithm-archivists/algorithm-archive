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
        }

        private Node root;

        public Tree(int depthCount, int childrenCount)
        {
            CreateTree(depthCount, childrenCount);
        }

        public void CreateTree(int depthCount, int childrenCount)
        {
            root = new Node
            {
                Id = 1
            };
            CreateAllChildren(root, depthCount, childrenCount);
        }

        public void StartDFSRecursive()
        {
            DFSRecursive(root);
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
                {
                    stack.Push(c);
                }
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
                {
                    queue.Enqueue(c);
                }
            }
        }

        private void CreateAllChildren(Node node, int rowCount, int childrenCount)
        {
            if (rowCount <= 1)
                return;

            for (int i = 0; i < childrenCount; i++)
            {
                node.Children.Add(new Node
                {
                    Id = node.Id * 10 + i + 1
                });
                CreateAllChildren(node.Children[i], rowCount - 1, childrenCount);
            }
        }

        private void DFSRecursive(Node node)
        {
            Console.WriteLine(node.Id);

            foreach (var c in node.Children)
            {
                DFSRecursive(c);
            }
        }
    }
}
