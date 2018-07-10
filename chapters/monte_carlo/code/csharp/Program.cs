using System;

// submitted by vitalii shvetsov (@sanstorik)
namespace MonteCarlo
{
    class Program
    {
        public static void Main(string[] args)
        {
            double piEstimate = new MonteCarlo(1, 1000000).Run();

            Console.WriteLine("Pi estimate = {0}", piEstimate);
            Console.WriteLine("Pi error = {0}", 100 * (Math.Abs(Math.PI - piEstimate)) / Math.PI);

            Console.ReadLine();
        }
    }
}
