// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace BogoSort
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("BogoSort");
            var listBogo = new List<int>() { 1, 2, 6, 4, 9, 54, 3, 2, 7, 15 };
            Console.Write("unsorted: ");
            foreach (var number in listBogo)
                Console.Write(number + " ");
            Console.WriteLine();
            listBogo = BogoSort.RunBogoSort(listBogo);
            Console.Write("sorted: ");
            foreach (var number in listBogo)
                Console.Write(number + " ");
            Console.WriteLine();
        }
    }
}
