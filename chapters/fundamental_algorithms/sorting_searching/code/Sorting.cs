// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace ArcaneAlgorithmArchive.FundamentalAlgorithms.SortingSearching
{
    public static class Sorting
    {
        public static List<T> BubbleSort<T>(List<T> list) where T : IComparable<T>
        {
            var length = list.Count;
            
            for (int i = 0; i < length; i++)
            {
                for (int j = 1; j < length; j++)
                {
                    if (list[j - 1].CompareTo(list[j]) > 0)
                    {
                        var temp = list[j - 1];
                        list[j - 1] = list[j];
                        list[j] = temp;
                    }
                }
            }

            return list;
        }

        public static List<T> BogoSort<T>(List<T> list) where T : IComparable<T>
        {
            while (!IsSorted(list))
                list = Shuffle(list, new Random());
            
            return list;
        }

        private static bool IsSorted<T>(List<T> list) where T : IComparable<T>
        {
            var sorted = true;

            for (int i = 0; i < list.Count - 1; i++)
            {
                if (!(0 >= list[i].CompareTo(list[i + 1])))
                    sorted = false;
            }
            if (!sorted)
            {
                sorted = true;
                for (int i = 0; i < list.Count - 1; i++)
                {
                    if (!(0 <= list[i].CompareTo(list[i + 1])))
                        sorted = false;
                }
            }

            return sorted;
        }

        private static List<T> Shuffle<T>(List<T> list, Random random)
        {
            for (int i = list.Count - 1; i > 0; i--)
            {
                var j = random.Next(0, i);
                var temp = list[i];
                list[i] = list[j];
                list[j] = temp;
            }
            return list;
        }
    }
}
