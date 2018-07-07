// submitted by Julian Schacher (jspp)
using System;
using System.Collections.Generic;

namespace BubbleSort
{
    public static class BubbleSort
    {
        public static List<T> RunBubbleSort<T>(List<T> list) where T : IComparable<T>
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
