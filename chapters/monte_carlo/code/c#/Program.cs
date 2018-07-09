using System;

namespace MonteCarlo
{
    class Program
    {
        private static Random random = new Random(47);
        private const int radius = 1;


        public static void Main(string[] args)
        {
            double piEstimate = MonteCarloMethod(1000000);

            Console.WriteLine("Pi estimate = {0}", piEstimate);
            Console.WriteLine("Pi error = {0}", 100 * (Math.Abs(Math.PI - piEstimate)) / Math.PI);

            Console.ReadLine();
        }


        private static double MonteCarloMethod(int samples)
        {
            int piCount = 0;

            for (int i = 0; i < samples; i++)
            {
                var randomX = random.NextDouble() * radius;
                var randomY = random.NextDouble() * radius;

                if (IsInCircle(randomX, randomY))
                {
                    piCount++;
                }
            }

            return 4 * piCount / (double)samples;
        }


        private static bool IsInCircle(double x, double y)
        {
            return x * x + y * y < radius * radius;
        }
    }
}
