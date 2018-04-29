// submitted by Julian Schacher (jspp) with help by gustorn
using System;
using System.Collections.Generic;

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

        public int ScalarProduct(Vector otherVector) => (this.x * otherVector.x) + (this.y * otherVector.y);

        public static bool operator==(Vector a, Vector b) => a.Equals(b);
        public static bool operator!=(Vector a, Vector b) => !(a == b);
        public static Vector operator+(Vector a, Vector b) => new Vector(a.x + b.x, a.y + b.y);
        public static Vector operator-(Vector a, Vector b) => new Vector(a.x - b.x, a.y - b.y);
    }

    public class JarvisMarch
    {
        public List<Vector> Run(List<Vector> points)
        {
            // Set the intial point to the first point of the list.
            var initialPoint = points[0];
            // Search for a better initial point. One where the x-position is the lowest.
            for (int i = 1; i < points.Count; i++)
            {
                if (points[i].x < initialPoint.x)
                {
                    initialPoint = points[i];
                }
            }
            // Add the initial point as the first point of the gift wrap.
            var giftWrap = new List<Vector>()
            {
                initialPoint
            };

            // Set previous point first to some point below the first initial point.
            var previousPoint = new Vector(initialPoint.x, initialPoint.y - 1);
            var currentPoint = initialPoint;

            var notWrapped = true;
            // Continue searching for the next point of the wrap until the wrap is completed.
            while (notWrapped)
            {
                // Search for next Point.
                // Set the first vector, which is currentPoint -> previousPoint.
                var firstVector = previousPoint - currentPoint;

                Vector? nextPoint = null;
                int scalarProduct = 0;
                for (int i = 1; i < points.Count; i++)
                {
                    // Set the second vector, which is currentPoint -> points[i](potential nextPoint).
                    var secondVector = points[i] - currentPoint;

                    // Calculate the current scalar product.
                    var tempScalarProduct = firstVector.ScalarProduct(secondVector);

                    // If there's currently no next Point or the current scalar product is smaller, set nextPoint to point[i].
                    if (nextPoint == null || tempScalarProduct < scalarProduct)
                    {
                        nextPoint = points[i];
                        scalarProduct = tempScalarProduct;
                    }
                }

                // Shift points and add/remove them from lists.
                previousPoint = currentPoint;
                currentPoint = nextPoint.Value;
                points.Remove(nextPoint.Value);
                giftWrap.Add(nextPoint.Value);
                // Check if the gift wrap is completed.
                if (nextPoint == giftWrap[0])
                    notWrapped = false;
            }

            return giftWrap;
        }
    }
}
