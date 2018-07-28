using System;

namespace MonteCarloIntegration
{
    public class MonteCarlo
    {
        public struct PiEstimate
        {
            public double Estimate { get; private set; }
            public double Error { get; private set; }

            public PiEstimate(double estimate, double error)
            {
                this.Estimate = estimate;
                this.Error = error;
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
            var error = Math.Abs(Math.PI - piEstimate);
            return new PiEstimate(piEstimate, error);
        }
    }
}
