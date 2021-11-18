using System;
using System.IO;

namespace Convolution1D
{
    public class Convolution1D
    {
        // Creates a sawtooth function with the given length.
        static double[] CreateSawtooth(int length)
        {
            var array = new double[length];
            for (var i = 0; i < length; i++)
                array[i] = (i + 1) / 200f;
            return array;
        }

        // Normalizes the given array.
        static void Normalize(double[] array)
        {
            var norm = Norm(array);
            for (var i = 0; i < array.Length; i++)
                array[i] /= norm;
        }

        // Calculates the norm of the array.
        static double Norm(double[] array)
        {
            var sum = 0.0;
            for (var i = 0; i < array.Length; i++)
                sum += Math.Pow(array[i], 2);
            return Math.Sqrt(sum);
        }

        // Modulus function which handles negative values properly.
        // Assumes that y >= 0.
        static int Mod(int x, int y) => ((x % y) + y) % y;

        static double[] ConvolveCyclic(double[] signal, double[] filter)
        {
            var outputSize = Math.Max(signal.Length, filter.Length);

            // Convolutional output.
            var output = new double[outputSize];
            var sum = 0.0;

            for (var i = 0; i < outputSize; i++)
            {
                for (var j = 0; j < outputSize; j++)
                {
                    if (Mod(i - j, outputSize) < filter.Length)
                    {
                        sum += signal[Mod(j - 1, outputSize)] * filter[Mod(i - j, outputSize)];
                    }
                }

                output[i] = sum;
                sum = 0.0;
            }

            return output;
        }

        static double[] ConvolveLinear(double[] signal, double[] filter, int outputSize)
        {
            // Convolutional output.
            var output = new double[outputSize];
            var sum = 0.0;

            for (var i = 0; i < outputSize; i++)
            {
                for (var j = Math.Max(0, i - filter.Length); j <= i; j++)
                {
                    if (j < signal.Length && (i - j) < filter.Length)
                    {
                        sum += signal[j] * filter[i - j];
                    }
                }

                output[i] = sum;
                sum = 0.0;
            }

            return output;
        }

        static void Main()
        {
            // Create sawtooth functions for x and y.
            var x = CreateSawtooth(200);
            var y = CreateSawtooth(200);

            // Normalization is not strictly necessary, but good practice.
            Normalize(x);
            Normalize(y);

            // Full convolution, output will be the size of x + y - 1.
            var fullLinearOutput = ConvolveLinear(x, y, x.Length + y.Length - 1);
            // Simple boundaries.
            var simpleLinearOutput = ConvolveLinear(x, y, x.Length);
            // Cyclic convolution.
            var cyclicOutput = ConvolveCyclic(x, y);

            // Output convolutions to different files for plotting.
            File.WriteAllText("full_linear.dat", String.Join(Environment.NewLine, fullLinearOutput));
            File.WriteAllText("simple_linear.dat", String.Join(Environment.NewLine, simpleLinearOutput));
            File.WriteAllText("cyclic.dat", String.Join(Environment.NewLine, cyclicOutput));
        }
    }
}

