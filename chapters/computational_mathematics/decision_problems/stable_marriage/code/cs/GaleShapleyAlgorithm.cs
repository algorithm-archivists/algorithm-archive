using System.Collections.Generic;

namespace ArcaneAlgorithmArchive.ComputationalMathematics.DecisionProblems.GaleShapley
{
    public static class GaleShapleyAlgorithm<TFollow, TLead>
        where TFollow : Person<TFollow, TLead>
        where TLead : Person<TLead, TFollow>
    {
        public static void RunGaleShapleyAlgorithm(List<TFollow> follows, List<TLead> leads)
        {
            CheckRequirements(follows, leads);
            
            // All follows are lonely.
            var lonelyFollows = new List<TFollow>(follows);
            // Carry on until there are no lonely follows anymore.
            while (lonelyFollows.Count > 0)
            {
                // Let the lead choose again and again.
                foreach (var lead in leads)
                {
                    ChoosePartner(lead, lonelyFollows);
                }
            }
        }

        private static void ChoosePartner(TLead lead, List<TFollow> lonelyFollows)
        {
            // Get the follows who want the lead (bachelors).
            var bachelors = GetBachelors(lead, lonelyFollows);

            if (bachelors.Count > 0)
            {
                // Give the existing partner a chance to prove theirself.
                if (lead.Partner != null)
                {
                    bachelors.Push(lead.Partner);
                    lonelyFollows.Add(lead.Partner);
                    lead.Partner = null;
                }
                
                // One partner for the beginning.
                lead.Partner = bachelors.Pop();
                // Let the lead choose.
                foreach (var bachelor in bachelors)
                {
                    foreach (var choice in lead.Choices)
                    {
                        if (choice == lead.Partner)
                            break;
                        if (choice == bachelor)
                        {
                            lead.Partner = bachelor;
                            break;
                        }
                    }
                }
                lonelyFollows.Remove(lead.Partner);
            }
        }
        
        private static Stack<TFollow> GetBachelors(TLead lead, List<TFollow> lonelyFollows)
        {
            var bachelors = new Stack<TFollow>();

            foreach (var lonelyFollow in lonelyFollows)
            {
                // Look if the lonely follow's top choice matches.
                if (lonelyFollow.IsCurrentTopChoice(lead))
                {
                    bachelors.Push(lonelyFollow);
                    lonelyFollow.CurrentTopChoiceIndex++;
                }
            }
            
            return bachelors;
        }
        
        // Check some requirements for the algorithm.
        // Not actually a part of the algorithm.
        public static void CheckRequirements(List<TFollow> follows, List<TLead> leads)
        {
            if (follows.Count != leads.Count)
                throw new System.Exception("Lead and Follows are not of the same count.");

            for (int i = 0; i < follows.Count; i++)
            {
                if (follows[i].Choices.Count != leads.Count)
                    throw new System.Exception(
                        $"The count of choices by the follow at index {i} in Follows is not equal to the count of Leads.");
                if (leads[i].Choices.Count != follows.Count)
                    throw new System.Exception(
                        $"The count of choices by the lead at index {i} in Leads is not equal to the count of Follows.");
            }
        }
    }
}
