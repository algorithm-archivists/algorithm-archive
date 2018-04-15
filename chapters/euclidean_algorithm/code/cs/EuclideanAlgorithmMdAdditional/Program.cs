// submitted by Julian Schacher (jspp)
using System;

namespace EuclideanAlgorithmMdAdditional
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("EuclideanAlgorithmMdAdditional");
            int checkMdAdditional = EuclideanAlgorithmMdAdditional.EuclidMod(64 * 67, 64 * 81);
            int checkMdAdditional2 = EuclideanAlgorithmMdAdditional.EuclidSub(128 * 12, 128 * 77);

            Console.WriteLine(checkMdAdditional);
            Console.WriteLine(checkMdAdditional2);
        }
    }
}
