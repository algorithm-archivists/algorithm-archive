using System;
using System.Collections.Generic;

namespace StableMarriageProblem
{
    public static class ListExtensions
    {
        public static IList<T> Shuffle<T>(this IList<T> list, Random rng)
        {
            for (var i = 0; i < list.Count; i++)
            {
                var j = rng.Next(i, list.Count);
                var tmp = list[i];
                list[i] = list[j];
                list[j] = tmp;
            }
            return list;
        }
    }
}
