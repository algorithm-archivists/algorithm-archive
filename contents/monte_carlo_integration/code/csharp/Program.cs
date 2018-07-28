using System;

namespace MonteCarloIntegration
{
    class Program
    {
        static void Main(string[] args)
        {
            var monteCarlo = new MonteCarlo();
            System.Console.WriteLine("Running with 10.000.000 samples.");
            var piEstimate = monteCarlo.Run(10000000);
            System.Console.WriteLine($"The estimate of pi is: {piEstimate.Estimate}");
            System.Console.WriteLine($"The error is: {piEstimate.Error}");

            System.Console.WriteLine("How many samples should be used?");
            var samples = 0;
            if (!int.TryParse(System.Console.ReadLine(), out samples))
                throw new System.Exception("You didn't input a valid number.");
            samples = Math.Abs(samples);
            System.Console.WriteLine($"Running with {samples} samples.");
            piEstimate = monteCarlo.Run(samples);
            System.Console.WriteLine($"The estimate of pi is: {piEstimate.Estimate}");
            System.Console.WriteLine($"The error is: {piEstimate.Error}");
        }
    }
}
