using System;

namespace MonteCarlo
{
    class Program
    {
        public static void Main(string[] args)
        {
            double piEstimate = new MonteCarlo(1).Run(1000000);

            Console.WriteLine("Pi estimate = {0}", piEstimate);
            Console.WriteLine("Pi error = {0}", 100 * (Math.Abs(Math.PI - piEstimate)) / Math.PI);

            Console.ReadLine();
        }
    }
}
