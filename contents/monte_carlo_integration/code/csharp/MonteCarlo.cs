using System;

namespace MonteCarloIntegration
{
    public class MonteCarlo
    {
        public struct PiEstimate
        {
            public double Estimate { get; private set; }
            public double PercentError { get; private set; }

            public PiEstimate(double estimate, double percentError)
            {
                this.Estimate = estimate;
                this.PercentError = percentError;
            }
        }

        public PiEstimate Run(int samples)
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

            var piEstimate = 4.0 * count / samples;
            var percentError = (piEstimate - Math.PI) / Math.PI * 100;
            return new PiEstimate(piEstimate, percentError);
        }
    }
}
