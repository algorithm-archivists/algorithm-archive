// submitted by Julian Schacher (jspp)
using System;

namespace TreeTraversal
{
    class Program
    {
        static void Main(string[] args)
        {
            Node root;
            Console.WriteLine("TreeTraversal");
            var tree = new Tree(3, 3);
            Console.WriteLine("StartDFSRecursive:");
            tree.StartDFSRecursive(root);
            Console.WriteLine("DFSStack:");
            tree.DFSStack(root);
            Console.WriteLine("DFSQueue:");
            tree.BFSQueue(root);
        }
    }
}
