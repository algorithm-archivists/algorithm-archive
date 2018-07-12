// submitted by Julian Schacher (jspp) with great help by gustorn
using System;
using System.Collections.Generic;
using System.Linq;

namespace JarvisMarch
{
    public struct Vector
    {
        public readonly int x;
        public readonly int y;

        public Vector(int xValue, int yValue)
        {
            this.x = xValue;
            this.y = yValue;
        }

        public override bool Equals(object obj) => obj is Vector v && this.x == v.x && this.y == v.y;
        public override int GetHashCode() => (17 * 23 + this.x) * 23 + this.y;

        public static bool operator==(Vector a, Vector b) => a.Equals(b);
        public static bool operator!=(Vector a, Vector b) => !(a == b);
    }

    public class JarvisMarch
    {
        public List<Vector> Run(List<Vector> points)
        {
            var convexHull = new List<Vector>();

            // Set the intial pointOnHull to the point of the list, where the x-position is the lowest.
            var pointOnHull = points.Aggregate((leftmost, current) => leftmost.x < current.x ? leftmost : current);

            // Continue searching for the next pointOnHull until the next pointOnHull is equal to the first point of the convex hull.
            do
            {
                convexHull.Add(pointOnHull);

                // Search for the next pointOnHull by looking which of the points is the next most outer point.
                pointOnHull = points.Aggregate((potentialNextPointOnHull, current) =>
                {
                    // Returns true, if potentialNextPointOnHull is equal to the current pointOnHull or if the current point is left of the line defined by pointOnHull and potentialNextPointOnHull.
                    if (potentialNextPointOnHull == pointOnHull || IsLeftOf(pointOnHull, potentialNextPointOnHull, current))
                        return current;
                    return potentialNextPointOnHull;
                });

                // Check if the gift wrap is completed.
            } while (pointOnHull != convexHull[0]);

            return convexHull;
        }

        // Returns true, if p is left of the line defined by a and b.
        private bool IsLeftOf(Vector a, Vector b, Vector p) => (b.x - a.x) * (p.y - a.y) > (p.x - a.x) * (b.y - a.y);
    }
}
