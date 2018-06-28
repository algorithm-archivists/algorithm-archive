// submitted by Julian Schacher (jspp)
using System;

namespace TreeTraversalMdAdditional
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("TreeTraversalMdAdditional");
            var treeMdAdditional = new TreeMdAdditional(3, 3);
            Console.WriteLine("StartDFSRecursive");
            treeMdAdditional.StartDFSRecursive();
            Console.WriteLine("StartDFSRecursivePostorder");
            treeMdAdditional.StartDFSRecursivePostorder();
            // Console.WriteLine("StartDFSRecursiveInorder (fail)");
            // treeMdAdditional.StartDFSRecursiveInorderBinary();
            treeMdAdditional = new TreeMdAdditional(3, 2);
            Console.WriteLine("StartDFSRecursiveInorder (succeed)");
            treeMdAdditional.StartDFSRecursiveInorderBinary();
        }
    }
}
