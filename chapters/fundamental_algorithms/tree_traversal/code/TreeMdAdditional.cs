// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace ArcaneAlgorithmArchive.FundamentalAlgorithms.TreeTraversal
{
    // This class recreates Tree and inlcudes changed or new methods needed for in-text-code.
    public class TreeMdAdditional
    {
        private class Node
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
        
        public void StartDFSRecursivePostorder()
        {
            DFSRecursivePostorder(root);
        }
        
        public void StartDFSRecursiveInorderBinary()
        {
            DFSRecursiveInorderBinary(root);
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

        private void DFSRecursivePostorder(Node node)
        {
            foreach (var c in node.Children)
            {
                DFSRecursivePostorder(c);
            }
            
            Console.WriteLine(node.Id);
        }
        
        private void DFSRecursiveInorderBinary(Node node)
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
