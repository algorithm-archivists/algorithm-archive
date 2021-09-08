// submitted by Julian Schacher (jspp)
using System;

namespace TreeTraversal
{
    class Program
    {
        static void Main(string[] args)
        {
            var tree = new Tree(2, 3);
            Console.WriteLine("[#] Recursive DFS:");
            tree.DFSRecursive();
            Console.WriteLine();

            Console.WriteLine("[#] Recursive Postorder DFS:");
            tree.DFSRecursivePostorder();
            Console.WriteLine();

            Console.WriteLine("[#] Stack-based DFS:");
            tree.DFSStack();
            Console.WriteLine();

            Console.WriteLine("[#] Queue-based BFS:");
            tree.BFSQueue();
            Console.WriteLine();

            tree = new Tree(3, 2);
            Console.WriteLine("[#] Recursive Inorder DFS for Binary Tree:");
            tree.DFSRecursiveInorderBinary();
            Console.WriteLine();
        }
    }
}
