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
        public class Node
        {
            public Node[] Children { get; set; } = new Node[2];
            public List<bool> BitString { get; set; } = new List<bool>();
            public int Weight { get; set; }
            public string Key { get; set; }

            public Node(string key, int weight)
            {
                this.Key = key;
                this.Weight =  weight;
            }
        }

        // Node with biggest value at the top.
        class NodePriorityList
        {
            public List<Node> Nodes { get; private set; } = new List<Node>();

            public NodePriorityList() { }
            public NodePriorityList(List<Node> nodes) => Nodes = nodes.OrderByDescending(n => n.Weight).ToList();

            public void AddNode(Node newNode)
            {
                if (Nodes.Count == 0)
                {
                    Nodes.Add(newNode);
                    return;
                }
                for (int i = Nodes.Count - 1; i >= 0; i--)
                {
                    if (Nodes[i].Weight > newNode.Weight)
                    {
                        Nodes.Insert(i + 1, newNode);
                        return;
                    }
                    else if (i == 0)
                        Nodes.Insert(0, newNode);
                }
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
                    currentNode = currentNode.Children[0];
                else
                    currentNode = currentNode.Children[1];

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
            var nodes = new List<Node>();
            foreach (var character in input)
            {
                var result = nodes.Where(n => n.Key[0] == character).SingleOrDefault();

                if (result == null)
                    nodes.Add(new Node(character.ToString(), 1));
                else
                    result.Weight++;
            }
            // Convert list of nodes to a NodePriorityList.
            var nodePriorityList = new NodePriorityList(nodes);
            nodes = nodePriorityList.Nodes;

            // Create Tree.
            while (nodes.Count > 1)
            {
                var parentNode = new Node("", 0);
                // Add the two nodes with the smallest weight to the parent node and remove them from the tree.
                for (int i = 0; i < 2; i++)
                {
                    parentNode.Children[i] = nodes.Last();
                    parentNode.Key += nodes.Last().Key;
                    parentNode.Weight += nodes.Last().Weight;

                    nodes.RemoveAt(nodes.Count - 1);
                };
                nodePriorityList.AddNode(parentNode);
            }

            return nodePriorityList.Nodes[0];
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
                    for (int i = 0; i < temp.Children.Count(); i++)
                    {
                        if (temp.Children[i] != null)
                        {
                            if (i == 0)
                            {
                                temp.Children[i].BitString.AddRange(temp.BitString);
                                temp.Children[i].BitString.Add(false);
                            }
                            else
                            {
                                temp.Children[i].BitString.AddRange(temp.BitString);
                                temp.Children[i].BitString.Add(true);
                            }

                            stack.Push(temp.Children[i]);
                        }
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