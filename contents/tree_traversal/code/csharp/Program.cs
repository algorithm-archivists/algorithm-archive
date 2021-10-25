using System;

namespace TreeTraversal
{
    class Program
    {
        static void Main(string[] args)
        {
            var tree = new Tree(2, 3);
            Console.WriteLine("[#]\nRecursive DFS:");
            tree.DFSRecursive();
            Console.WriteLine();

            Console.WriteLine("[#]\nRecursive Postorder DFS:");
            tree.DFSRecursivePostorder();
            Console.WriteLine();

            Console.WriteLine("[#]\nStack-based DFS:");
            tree.DFSStack();
            Console.WriteLine();

            Console.WriteLine("[#]\nQueue-based BFS:");
            tree.BFSQueue();
            Console.WriteLine();

            tree = new Tree(3, 2);
            Console.WriteLine("[#]\nRecursive Inorder DFS for Binary Tree:");
            tree.DFSRecursiveInorderBinary();
            Console.WriteLine();
        }
    }
}
