// submitted by Julian Schacher (jspp)
using System;

namespace EuclideanAlgorithm
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("EuclideanAlgorithm");
            int check = EuclideanAlgorithm.EuclidMod(64 * 67, 64 * 81);
            int check2 = EuclideanAlgorithm.EuclidSub(128 * 12, 128 * 77);

            Console.WriteLine(check);
            Console.WriteLine(check2);
        }
    }
}
