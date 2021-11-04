using System;

namespace ApproximateCounting
{
    class ApproximateCounting
    {
        static readonly Random Rng = new();

        /// <param name="v">value in register</param>
        /// <param name="a">scaling value for the logarithm based on Morris's paper</param>
        /// <returns>N(v,a) the approximate count for the given values</returns>
        static double N(int v, double a)
        {
            return a * Math.Pow(1 + 1 / a, v - 1);
        }
        
        /// <param name="v">value in register</param>
        /// <param name="a">scaling value for the logarithm based on Morris's paper</param>
        /// <returns>Returns the new value for v</returns>
        static int Increment(int v, double a)
        {
            var delta = 1 / (N(v + 1, a) - N(v, a));

            if (Rng.NextDouble() <= delta)
                return v + 1;
            else
                return v;
        }

        /// <summary>
        /// This function simulates approximate counting
        /// </summary>
        /// <param name="noItems">number of items to count and loop over</param>
        /// <param name="a">a scaling value for the logarithm based on Morris's paper</param>
        /// <returns>It returns n(v,a), the approximate count</returns>
        static double ApproximateCount(int noItems, double a)
        {
            var v = 0;
            for (var i = 0; i < noItems; i++)
            {
                v = Increment(v, a);
            }

            return N(v, a);
        }

        /// <param name="noTrials">the number of counting trials</param>
        /// <param name="noItems">the number of items to count to</param>
        /// <param name="a">a scaling value for the logarithm based on Morris's paper</param>
        /// <param name="threshold">the maximum percent error allowed</param>
        /// <returns>"passed" or "failed" depending on the test result</returns>
        static string TextApproximateCount(int noTrials, int noItems, double a, double threshold)
        {
            var sum = 0.0;
            for (var i = 0; i < noTrials; i++)
                sum += ApproximateCount(noItems, a);

            var avg = sum / noTrials;

            return Math.Abs((avg - noItems) / noItems) < threshold ? "passed" : "failed";
        }

        static void Main()
        {
            Console.WriteLine("[#]\nCounting Tests, 100 trials");
            Console.WriteLine($"[#]\nTesting 1,000, a = 30, 1% error : {TextApproximateCount(100, 1_000, 30, 0.1)}");
            Console.WriteLine($"[#]\nTesting 12,345, a = 10, 10% error : {TextApproximateCount(100, 12_345, 10, 0.1)}");
            Console.WriteLine(
                $"[#]\nTesting 222,222, a = 0.5, 20% error : {TextApproximateCount(100, 222_222, 0.5, 0.2)}");
        }
    }
}

