using System;
using System.Collections.Generic;

namespace TreeTraversal
{
    public class Tree
    {
        public int Id { get; private set; }
        private List<Tree> _children = new List<Tree>();

        public Tree(int depthCount, int childrenCount)
        {
            Id = 1;

            if (!(depthCount <= 1))
            {
                for (int i = 0; i < childrenCount; i++)
                    _children.Add(new Tree(Id * 10 + i + 1, depthCount - 1, childrenCount));
            }
        }

        private Tree(int id, int depthCount, int childrenCount)
        {
            Id = id;

            if (!(depthCount <= 1))
            {
                for (int i = 0; i < childrenCount; i++)
                    _children.Add(new Tree(Id * 10 + i + 1, depthCount - 1, childrenCount));
            }
        }

        public void DFSRecursive()
        {
            DFSRecursive(this);

            void DFSRecursive(Tree tree)
            {
                Console.WriteLine(tree.Id);

                foreach (var c in tree._children)
                    DFSRecursive(c);
            }
        }

        public void DFSRecursivePostorder()
        {
            DFSRecursivePostorder(this);

            void DFSRecursivePostorder(Tree tree)
            {
                foreach (var c in tree._children)
                    DFSRecursivePostorder(c);

                Console.WriteLine(tree.Id);
            }
        }

        public void DFSRecursiveInorderBinary()
        {
            DFSRecursiveInorderBinary(this);

            void DFSRecursiveInorderBinary(Tree tree)
            {
                switch (tree._children.Count)
                {
                    case 2:
                        DFSRecursiveInorderBinary(tree._children[0]);
                        Console.WriteLine(tree.Id);
                        DFSRecursiveInorderBinary(tree._children[1]);
                        break;
                    case 1:
                        DFSRecursiveInorderBinary(tree._children[0]);
                        Console.WriteLine(tree.Id);
                        break;
                    case 0:
                        Console.WriteLine(tree.Id);
                        break;
                    default:
                        throw new Exception("Not binary tree!");
                }
            }
        }

        public void DFSStack()
        {
            var stack = new Stack<Tree>();
            stack.Push(this);

            while (stack.Count != 0)
            {
                Console.WriteLine(stack.Peek().Id);
                var temp = stack.Pop();

                foreach (var c in temp._children)
                    stack.Push(c);
            }
        }

        public void BFSQueue()
        {
            var queue = new Queue<Tree>();
            queue.Enqueue(this);

            while (queue.Count != 0)
            {
                Console.WriteLine(queue.Peek().Id);
                var temp = queue.Dequeue();

                foreach (var c in temp._children)
                    queue.Enqueue(c);
            }
        }
    }
}
