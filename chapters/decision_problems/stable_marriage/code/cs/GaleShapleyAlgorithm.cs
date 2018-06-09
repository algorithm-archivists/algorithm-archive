// submitted by Julian Schacher (jspp) with great help by gustorn and Marius Becker
using System.Collections.Generic;

namespace StableMarriageProblem
{
    public static class GaleShapleyAlgorithm<TFollow, TLead>
        where TFollow : Person<TFollow, TLead>
        where TLead : Person<TLead, TFollow>
    {
        public static void RunGaleShapleyAlgorithm(List<TFollow> follows, List<TLead> leads)
        {
            // All follows are lonely.
            var lonelyFollows = new List<TFollow>(follows);

            // Carry on until there are no lonely follows anymore.
            while (lonelyFollows.Count > 0)
            {
                // Let every lonely follow propose to their current top choice.
                foreach (var lonelyFollow in lonelyFollows)
                {
                    lonelyFollow.ProposeToNext();
                }

                // Look which follows have a partner now and which don't.
                var newLonelyFollows = new List<TFollow>();
                foreach (var follow in follows)
                {
                    if (follow.Partner == null)
                        newLonelyFollows.Add(follow);
                }
                lonelyFollows = newLonelyFollows;
            }
        }
    }
}
