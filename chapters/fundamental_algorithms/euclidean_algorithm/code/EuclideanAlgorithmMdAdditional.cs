// submitted by Julian Schacher (jspp)
namespace ArcaneAlgorithmArchive.FundamentalAlgorithms.EuclideanAlgorithm
{
    public class EuclideanAlgorithmMdAdditional
    {
        public static int EuclidSub(int a, int b)
        {
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
