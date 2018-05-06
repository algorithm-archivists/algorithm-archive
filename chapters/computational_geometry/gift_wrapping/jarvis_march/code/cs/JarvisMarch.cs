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
        public static Vector operator+(Vector a, Vector b) => new Vector(a.x + b.x, a.y + b.y);
        public static Vector operator-(Vector a, Vector b) => new Vector(a.x - b.x, a.y - b.y);
    }

    public class JarvisMarch
    {
        public List<Vector> Run(List<Vector> points)
        {
            var convexHull = new List<Vector>();

            // Set the intial point to the point of the list, where the x-position is the lowest.
            var initialPoint = points.Aggregate((leftmost, current) => leftmost.x < current.x ? leftmost : current);

            convexHull.Add(initialPoint);
            var currentPoint = initialPoint;
            var nextPoint = currentPoint;

            // Continue searching for the next point of the convex hull until the next point of the convex hull is equal to the first point of the convex hull.
            do
            {

                // Search for the next point by looking which of the remaining points is the next most outer point (or left point if viewed from currentPoint).
                nextPoint = points.Aggregate((potentialNextPoint, current) =>
                {
                    if (IsLeftOf(currentPoint, potentialNextPoint, current))
                        return current;
                    return potentialNextPoint;
                });

                convexHull.Add(nextPoint);
                points.Remove(nextPoint);
                currentPoint = nextPoint;

                // Check if the gift wrap is completed.
            } while (nextPoint != convexHull[0]);

            return convexHull;
        }

        // Returns true, if p is more left than b, if viewed from a.
        private bool IsLeftOf(Vector a, Vector b, Vector p)
        {
            return (b.x - a.x) * (p.y - a.y) > (p.x - a.x) * (b.y - a.y);
        }
    }
}
