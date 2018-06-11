// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace TreeTraversalMdAdditional
{
    // This class recreates Tree and inlcudes changed or new methods needed for in-text-code.
    // Therefor this file disregards coding standards and best practices.
    public class TreeMdAdditional
    {
        public class Node
        {
            public List<Node> Children { get; set; } = new List<Node>();
            public int Id { get; set; }
        }

        private Node root;

        public void CreateTree(int depthCount, int childrenCount)
        {
            root = new Node
            {
                Id = 1
            };
            CreateAllChildren(root, depthCount, childrenCount);
        }

        public TreeMdAdditional(int depthCount, int childrenCount)
        {
            CreateTree(depthCount, childrenCount);
        }

        public void StartDFSRecursive()
        {
            DFSRecursive(root);
        }

        public void StartDFSRecursivePostorder()
        {
            DFSRecursivePostorder(root);
        }

        public void StartDFSRecursiveInorderBinary()
        {
            DFSRecursiveInorderBinary(root);
        }

        public void DFSRecursive(Node node)
        {
            // Here we are doing something...
            Console.WriteLine(node.Id);

            foreach (var c in node.Children)
            {
                DFSRecursive(c);
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

        public void DFSRecursivePostorder(Node node)
        {
            foreach (var c in node.Children)
            {
                DFSRecursivePostorder(c);
            }

            // Here we are doing something...
            Console.WriteLine(node.Id);
        }

        // This assumes only 2 children
        public void DFSRecursiveInorderBinary(Node node)
        {
            if (node.Children.Count > 2)
            {
                throw new Exception("Not binary tree!");
            }

            if (node.Children.Count > 0)
            {
                DFSRecursiveInorderBinary(node.Children[0]);
                Console.WriteLine(node.Id);
                DFSRecursiveInorderBinary(node.Children[1]);
            }
            else
            {
                Console.WriteLine(node.Id);
            }
        }
    }
}