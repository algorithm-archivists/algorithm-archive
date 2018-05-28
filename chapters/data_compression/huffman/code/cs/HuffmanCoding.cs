// submitted by Julian Schacher (jspp) with help by gustorn.
using System;
using System.Collections.Generic;
using System.Linq;

namespace HuffmanCoding
{
    public class EncodingResult 
    {
        public List<bool> BitString { get; set; }
        public Dictionary<char, List<bool>> Dictionary { get; set; }
        public HuffmanCoding.Node Tree { get; set; }

        public EncodingResult(List<bool> bitString, Dictionary<char, List<bool>> dictionary, HuffmanCoding.Node tree)
        {
            this.BitString = bitString;
            this.Dictionary = dictionary;
            this.Tree = tree;
        }
    }

    public static class HuffmanCoding
    {
        // The Node class used for the Huffman Tree.
        public class Node : IComparable<Node>
        {
            public Node LeftChild { get; set; }
            public Node RightChild { get; set; }
            public List<bool> BitString { get; set; } = new List<bool>();
            public int Weight { get; set; }
            public string Key { get; set; }
            
            // Creates a leaf. So just a node is created with the given values.
            public static Node CreateLeaf(char key, int weight) => new Node(key.ToString(), weight, null, null);
            // Creates a branch. Here a node is created by adding the keys and weights of both childs together.
            public static Node CreateBranch(Node leftChild, Node rightChild) => new Node(leftChild.Key + rightChild.Key, leftChild.Weight + rightChild.Weight, leftChild, rightChild);
            private Node(string key, int weight, Node leftChild, Node rightChild)
            {
                this.Key = key;
                this.Weight =  weight;
                this.LeftChild = leftChild;
                this.RightChild = rightChild;
            }

            public int CompareTo(Node other) => this.Weight - other.Weight;
        }

        // Node with biggest value at the top.
        class NodePriorityList
        {
            public int Count => nodes.Count;

            private List<Node> nodes = new List<Node>();

            public NodePriorityList() { }
            public NodePriorityList(List<Node> givenNodes)
            {
                this.nodes = givenNodes.ToList();
                this.nodes.Sort();
            }

            public void Add(Node newNode)
            {
                var index = ~this.nodes.BinarySearch(newNode);
                if (index == this.nodes.Count)
                {
                    this.nodes.Add(newNode);
                    return;
                }
                this.nodes.Insert(~index, newNode);
            }

            public Node Pop()
            {
                var first = this.nodes.First();
                if (first != null)
                    this.nodes.Remove(first);
                return first;
            }
        }

        public static EncodingResult Encode(string input)
        {
            var root = CreateTree(input);
            var dictionary = CreateDictionary(root);
            var bitString = CreateBitString(input, dictionary);

            return new EncodingResult(bitString, dictionary, root);
        }

        public static string Decode(EncodingResult result)
        {
            var output = "";
            Node currentNode = result.Tree;
            foreach (var boolean in result.BitString)
            {
                // Go down the tree.
                if (!boolean)
                    currentNode = currentNode.LeftChild;
                else
                    currentNode = currentNode.RightChild;

                // Check if it's a leaf node.
                if (currentNode.Key.Count() == 1)
                {                    
                    output += currentNode.Key;
                    currentNode = result.Tree;
                }
            }
            return output;
        }

        private static Node CreateTree(string input)
        {
            // Create a List of all characters and their count in input by putting them into nodes.
            var nodes = input
                .GroupBy(c => c)
                .Select(n => Node.CreateLeaf(n.Key, n.Count()))
                .ToList();

            // Convert list of nodes to a NodePriorityList.
            var nodePriorityList = new NodePriorityList(nodes);

            // Create Tree.
            while (nodePriorityList.Count > 1)
            {
                // Pop the two nodes with the smallest weights from the nodePriorityList and create a parentNode with the CreateBranch method. (This method adds the keys and weights of the childs together.)
                var leftChild = nodePriorityList.Pop();
                var rightChild = nodePriorityList.Pop();
                var parentNode = Node.CreateBranch(leftChild, rightChild);

                nodePriorityList.Add(parentNode);
            }

            return nodePriorityList.Pop();
        }

        private static Dictionary<char, List<bool>> CreateDictionary(Node root)
        {
            var dictionary = new Dictionary<char, List<bool>>();

            var stack = new Stack<Node>();
            stack.Push(root);
            Node temp;

            while (stack.Count != 0)
            {
                temp = stack.Pop();

                if (temp.Key.Count() == 1)
                    dictionary.Add(temp.Key[0], temp.BitString);
                else
                {
                    if (temp.LeftChild != null)
                    {
                        temp.LeftChild.BitString.AddRange(temp.BitString);
                        temp.LeftChild.BitString.Add(false);
                        stack.Push(temp.LeftChild);
                    }
                    if (temp.RightChild != null)
                    {
                        temp.RightChild.BitString.AddRange(temp.BitString);
                        temp.RightChild.BitString.Add(true);
                        stack.Push(temp.RightChild);
                    }
                }
           }

           return dictionary;
        }

        private static List<bool> CreateBitString(string input, Dictionary<char, List<bool>> dictionary)
        {
            var bitString = new List<bool>();
            foreach (var character in input)
                bitString.AddRange(dictionary[character]);

            return bitString;
        }
    }
}