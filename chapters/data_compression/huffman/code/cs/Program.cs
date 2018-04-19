// submitted by Julian Schacher (jspp)
using System.Collections;
using System.Collections.Generic;

namespace HuffmanCoding
{
    class Program
    {
        static void Main(string[] args)
        {
            var result = HuffmanCoding.Encode("aaaabbbccd");
            // Print dictionary.
            foreach (var entry in result.Dictionary)
            {
                var bitString = "";
                foreach (var value in entry.Value)
                {
                    if (value)
                        bitString += "1";
                    else
                        bitString += "0";
                }
                System.Console.WriteLine(entry.Key + " " + bitString);
            }
            // Print bitString.
            var readableBitString = "";
            foreach (var boolean in result.BitString)
            {
                if (boolean)
                    readableBitString += "1";
                else
                    readableBitString += "0";
            }
            System.Console.WriteLine(readableBitString);

            var originalString = HuffmanCoding.Decode(result);
            System.Console.WriteLine(originalString);
        }
    }
}