// submitted by Julian Schacher (jspp)
using System;

namespace EuclideanAlgorithm
{
    public static class EuclideanAlgorithm
    {
        public static int EuclidSub(int a, int b)
        {
            // Math.Abs for negative number support
            a = Math.Abs(a);
            b = Math.Abs(b);

            while (a != b)
            {
                if (a > b)
                    a = a - b;
                else
                    b = b - a;
            }

            return a;
        }

        public static int EuclidMod(int a, int b)
        {
            // Math.Abs for negative number support
            a = Math.Abs(a);
            b = Math.Abs(b);

            while (b != 0)
            {
                var temp = b;
                b = a % b;
                a = temp;
            }

            return a;
        }
    }
}
