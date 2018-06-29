// submitted by Julian Schacher (jspp) with great help by gustorn
using System;
using System.Collections.Generic;

namespace JarvisMarch
{
    class Program
    {
        static void Main(string[] args)
        {
            System.Console.WriteLine("JarvisMarch");
            // Example list of points.
            // The points are represented by vectors here, but that doesn't really matter.
            var points = new List<Vector>()
            {
                new Vector(1, 3),
                new Vector(2, 4),
                new Vector(4, 0),
                new Vector(1, 0),
                new Vector(0, 2),
                new Vector(2, 2),
                new Vector(3, 4),
                new Vector(3, 1),
            };
            var jarvisMarch = new JarvisMarch();
            var giftWrap = jarvisMarch.Run(points);

            // Print the points of the gift wrap.
            foreach (var point in giftWrap)
                System.Console.WriteLine($"{point.x}, {point.y}");
        }
    }
}
