using System;
using System.Collections.Generic;
using System.Text;

namespace MonteCarlo
{
    class MonteCarlo
    {
        private static Random random = new Random(47);
        private readonly int radius = 1;


        public MonteCarlo(int radius)
        {
            this.radius = radius;
        }


        public double Run(int samples)
        {
            int piCount = 0;

            for (int i = 0; i < samples; i++)
            {
                var randomX = random.NextDouble() * radius;
                var randomY = random.NextDouble() * radius;

                if (IsInCircle(randomX, randomY)) piCount++;
            }

            return 4 * piCount / (double)samples;
        }


        private bool IsInCircle(double x, double y) => x * x + y * y < radius * radius;
    }
}
