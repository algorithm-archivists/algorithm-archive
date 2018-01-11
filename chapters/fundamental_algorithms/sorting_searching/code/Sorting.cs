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
    }
}
