// Submitted by Julian Schacher (jspp) with help and enhancements by Marius Becker and gustorn.
using System.Collections.Generic;

namespace ArcaneAlgorithmArchive.ComputationalMathematics.DecisionProblems.GaleShapley
{
    public class GaleShapleyAlgorithm<follow, lead>
        where follow : Person<follow, lead>
        where lead : Person<lead, follow>
    {
        public List<follow> Follows { get; set; }
        public List<lead> Leads { get; set; }

        private List<follow> _lonelyFollows;

        public void RunGaleShapleyAlgorithm()
        {
            CheckRequirements();
            
            // All follows are lonely.
            _lonelyFollows = new List<follow>(Follows);
            // Carry on until there are no lonely follows anymore.
            while (_lonelyFollows.Count > 0)
            {
                // Let the lead choose again and again.
                foreach (var lead in Leads)
                {
                    ChoosePartner(lead);
                }
            }
        }

        private void ChoosePartner(lead lead)
        {
            // Get the follow who want the lead (bachelors).
            var bachelors = GetBachelors(lead);

            if (bachelors.Count > 0)
            {
                // Give the existing partner a chance to prove theirself.
                if (lead.Partner != null)
                {
                    bachelors.Push(lead.Partner);
                    _lonelyFollows.Add(lead.Partner);
                    lead.Partner = null;
                }
                
                // One partner for the beginning.
                lead.Partner = bachelors.Pop();
                // Let the lead choose.
                foreach (var bachelor in bachelors)
                {
                    foreach (var choice in lead.Choices.Values)
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
                _lonelyFollows.Remove(lead.Partner);
            }
        }
        
        private Stack<follow> GetBachelors(lead lead)
        {
            var bachelors = new Stack<follow>();

            foreach (var lonelyFollow in _lonelyFollows)
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
        public void CheckRequirements()
        {
            if (Follows.Count != Leads.Count)
                throw new System.Exception("Lead and Follows are not of the same count.");

            for (int i = 0; i < Follows.Count; i++)
            {
                if (Follows[i].Choices.Count != Leads.Count)
                    throw new System.Exception(
                        $"The count of choices by the follow at index {i} in Follows is not equal to the count of Leads.");
                if (Leads[i].Choices.Count != Follows.Count)
                    throw new System.Exception(
                        $"The count of choices by the lead at index {i} in Leads is not equal to the count of Follows.");
            }
        }
    }
}
