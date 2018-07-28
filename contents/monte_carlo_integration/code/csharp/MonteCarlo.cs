using System;

namespace MonteCarloIntegration
{
    public class MonteCarlo
    {
        public struct RunOutput
        {
            public double PiEstimate { get; private set; }
            public double Error { get; private set; }

            public RunOutput(double piEstimate, double error)
            {
                this.PiEstimate = piEstimate;
                this.Error = error;
            }
        }

        public RunOutput Run(int samples)
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
            return new RunOutput(piEstimate, error);
        }
    }
}
