// submitted by vitalii shvetsov (@sanstorik)
using System;
using System.Collections.Generic;
using System.Text;

namespace MonteCarlo
{
    class MonteCarlo
    {
        private static Random random = new Random(47);
        private int Radius { get; set; }
        private int Samples { get; set; }


        public MonteCarlo(int radius = 1, int samples = 1000000)
        {
            Radius = radius;
            Samples = samples;
        }


        public double Run()
        {
            int piCount = 0;

            for (int i = 0; i < Samples; i++)
            {
                var randomX = random.NextDouble() * Radius;
                var randomY = random.NextDouble() * Radius;

                if (IsInCircle(randomX, randomY)) piCount++;
            }

            return 4 * piCount / (double)Samples;
        }


        private bool IsInCircle(double x, double y) => x * x + y * y < Radius * Radius;
    }
}
