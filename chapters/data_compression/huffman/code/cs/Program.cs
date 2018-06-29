// submitted by Julian Schacher (jspp), thanks to gustorn for the help
using System.Collections;
using System.Collections.Generic;

namespace HuffmanCoding
{
    class Program
    {
        static void Main(string[] args)
        {
            var huffmanCoding = new HuffmanCoding();

            var result = huffmanCoding.Encode("bibbity bobbity");
            // The bitStrings are just strings and provide no compression. Look in HuffmanCoding.cs for explanation.
            // Print dictionary.
            foreach (var entry in result.Dictionary)
                System.Console.WriteLine($"{entry.Key} {entry.Value}");
            // Print BitString.
            System.Console.WriteLine($"{result.BitString} count: {result.BitString.Length}");

            var originalString = huffmanCoding.Decode(result);
            System.Console.WriteLine(originalString);
        }
    }
}