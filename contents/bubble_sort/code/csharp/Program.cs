// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace BubbleSort
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("BubbleSort");
            var listBubble = new List<int>() { 1, 2, 6, 4, 9, 54, 3, 2, 7, 15 };
            Console.Write("unsorted: ");
            foreach (var number in listBubble)
                Console.Write(number + " ");
            Console.WriteLine();
            listBubble = BubbleSort.RunBubbleSort(listBubble);
            Console.Write("sorted: ");
            foreach (var number in listBubble)
                Console.Write(number + " ");
            Console.WriteLine();
        }
    }
}
