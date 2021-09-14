using System;

namespace ApproximateCounting
{
    class ApproximateCounting
    {
        static readonly Random Rng = new();

        // This function takes 
        //     - v: value in register
        //     - a: a  scaling value for the logarithm based on Morris's paper
        // It returns n(v,a), the approximate count
        static double N(int v, double a)
        {
            return a * Math.Pow(1 + 1 / a, v - 1);
        }

        // This function takes
        //    - v: value in register
        //    - a: a scaling value for the logarithm based on Morris's paper
        // It returns a new value for v
        static int Increment(int v, double a)
        {
            var delta = 1 / (N(v + 1, a) - N(v, a));

            if (Rng.NextDouble() <= delta)
                return v + 1;
            else
                return v;
        }

        // This simulates counting and takes
        //     - noItems: number of items to count and loop over
        //     - a: a scaling value for the logarithm based on Morris's paper
        // It returns n(v,a), the approximate count
        static double ApproximateCount(int noItems, double a)
        {
            var v = 0;
            for (var i = 0; i < noItems; i++)
            {
                v = Increment(v, a);
            }
            return N(v, a);
        }

        // This function takes
        //     - noTrials: the number of counting trials
        //     - noItems: the number of items to count to
        //     - a: a scaling value for the logarithm based on Morris's paper
        //     - threshold: the maximum percent error allowed
        // It returns a "pass" / "fail" test value
        static string TextApproximateCount(int noTrials, int noItems, double a, double threshold)
        {
            var sum = 0.0;
            for (var i = 0; i < noTrials; i++)
                sum += ApproximateCount(noItems, a);
            
            var avg = sum / noTrials;
            
            return Math.Abs((avg - noItems) / noItems) < threshold ? "pass" : "fail";
        }

        static void Main()
        {
            Console.WriteLine("Counting Tests, 100 trials");
            Console.WriteLine($"Testing 1,000, a = 30, 1% error : {TextApproximateCount(100, 1_000, 30, 0.1)}");
            Console.WriteLine($"Testing 12,345, a = 10, 1% error : {TextApproximateCount(100, 12_345, 10, 0.1)}");
            Console.WriteLine($"Testing 222,222, a = 0.5, 10% error : {TextApproximateCount(100, 222_222, 0.5, 0.2)}");
        }
    }
}
