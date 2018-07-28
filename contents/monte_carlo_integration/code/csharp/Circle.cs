using System;

namespace MonteCarloIntegration
{
    public struct Point
    {
        public double X { get; set; }
        public double Y { get; set; }

        public Point(double x, double y)
        {
            this.X = x;
            this.Y = y;
        }
    }

    public class Circle
    {
        public double Radius { get; private set; }

        public Circle(double radius) => this.Radius = Math.Abs(radius);

        public bool IsInMe(Point point) => point.X * point.X + point.Y * point.Y < Radius * Radius;
    }
}
