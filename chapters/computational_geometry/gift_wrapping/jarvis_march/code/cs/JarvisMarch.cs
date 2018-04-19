// submitted by Julian Schacher (jspp)
using System.Collections.Generic;

namespace JarvisMarch
{
    public class Vector
    {
        public int X { get; set; }
        public int Y { get; set; }

        public Vector(int xValue, int yValue)
        {
            this.X = xValue;
            this.Y = yValue;
        }
    }

    public static class JarvisMarch
    {
        public static List<Vector> RunJarvisMarch(List<Vector> points)
        {
            // Set the intial point to the first point of the list.
            var initialPoint = points[0];
            // Search for a better initial point. One where the x-position is the lowest.
            for (int i = 1; i < points.Count; i++)
            {
                if (points[i].X < initialPoint.X)
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
            var previousPoint = new Vector(initialPoint.X, initialPoint.Y - 1);
            var currentPoint = initialPoint;

            var notWrapped = true;
            // Continue searching for the next point of the wrap until the wrap is completed.
            while (notWrapped)
            {
                // Search for next Point.
                // Set the first vector, which is currentPoint -> previousPoint.
                var firstVector = new Vector(previousPoint.X - currentPoint.X, previousPoint.Y - currentPoint.Y);

                Vector nextPoint = null;
                int scalarProduct = 0;
                for (int i = 1; i < points.Count; i++)
                {
                    // Set the second vector, which is currentPoint -> points[i](potential nextPoint).
                    var secondVector = new Vector(points[i].X - currentPoint.X, points[i].Y - currentPoint.Y);

                    // Calculate the current scalar product.
                    var tempScalarProduct = (firstVector.X * secondVector.X) + (firstVector.Y * secondVector.Y);

                    // If there's currently no next Point or the current scalar product is smaller, set nextPoint to point[i].
                    if (nextPoint == null || tempScalarProduct < scalarProduct)
                    {
                        nextPoint = points[i];
                        scalarProduct = tempScalarProduct;
                    }
                }

                // Shift points and add/remove them from lists.
                previousPoint = currentPoint;
                currentPoint = nextPoint;
                points.Remove(nextPoint);
                giftWrap.Add(nextPoint);
                // Check if the gift wrap is completed.
                if (nextPoint == giftWrap[0])
                    notWrapped = false;
            }

            return giftWrap;
        }
    }
}
