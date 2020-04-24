using System;

namespace MonteCarloIntegration
{
    public class MonteCarlo
    {
        public double Run(int samples)
        {
            var circle = new Circle(1.0);
            var count = 0;
            var random = new Random();

            for (int i = 0; i < samples; i++)
            {
                var point = new Point(random.NextDouble(), random.NextDouble());
                if (circle.IsInMe(point))
                    count++;
            }

            return 4.0 * count / samples;
        }
    }
}
