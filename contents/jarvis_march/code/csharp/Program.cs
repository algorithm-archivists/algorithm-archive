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
                new Vector(-5, 2),
                new Vector(5, 7),
                new Vector(-6, -12),
                new Vector(-14, -14),
                new Vector(9, 9),
                new Vector(-1, -1),
                new Vector(-10, 11),
                new Vector(-6, 15),
                new Vector(-6, -8),
                new Vector(15, -9),
                new Vector(7, -7),
                new Vector(-2, -9),
                new Vector(6, -5),
                new Vector(0, 14),
                new Vector(2, 8),
            };
            var jarvisMarch = new JarvisMarch();
            var giftWrap = jarvisMarch.Run(points);

            // Print the points of the gift wrap.
            foreach (var point in giftWrap)
                System.Console.WriteLine($"{point.x}, {point.y}");
        }
    }
}
