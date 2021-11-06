// submitted by Julian Schacher (jspp)
using System;

namespace TreeTraversal
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("TreeTraversal");
            var tree = new Tree(3, 3);
            Console.WriteLine("DFSRecursive:");
            tree.DFSRecursive();
            Console.WriteLine("DFSStack:");
            tree.DFSStack();
            Console.WriteLine("BFSQueue:");
            tree.BFSQueue();
            Console.WriteLine("DFSRecursivePostorder");
            tree.DFSRecursivePostorder();

            // Uncommenting the following 2 lines will result in an exception thrown because at least one Node of the Tree has more than 2 children and therefor a DFSRecursiveInorderBinary doesn't work.
            // Console.WriteLine("DFSRecursiveInorder (fail)");
            // tree.DFSRecursiveInorderBinary();
            tree = new Tree(3, 2);
            Console.WriteLine("DFSRecursiveInorder (succeed)");
            tree.DFSRecursiveInorderBinary();
        }
    }
}
