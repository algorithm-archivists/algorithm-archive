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
    class MainClass
    {
        public static void Main(string[] args)
        {
            int check = EuclideanAlgorithm.EuclidMod(64 * 67, 64 * 81);
            int check2 = EuclideanAlgorithm.EuclidSub(128 * 12, 128 * 77);
            
            Console.WriteLine(check);
            Console.WriteLine(check2);
        }
    }
}
