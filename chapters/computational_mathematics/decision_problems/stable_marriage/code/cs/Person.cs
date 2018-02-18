using System.Collections.Generic;

namespace ArcaneAlgorithmArchive.ComputationalMathematics.DecisionProblems.GaleShapley
{
    public class Person<TSelf, TPref>
        where TSelf : Person<TSelf, TPref>
        where TPref : Person<TPref, TSelf>
    {
        private TPref _partner;
        public TPref Partner
        {
            get => _partner;
            // Set partner and partner of partner.
            set
            {
                if (value != _partner)
                {
                    if (value == null)
                    {
                        var oldPartner = _partner;
                        _partner = null;
                        oldPartner.Partner = null;
                    }
                    else
                    {
                        if (_partner != null)
                            _partner.Partner = null;
                        
                        _partner = value;
                        _partner.Partner = this as TSelf;
                    }
                }
            }
        }
        public IList<TPref> Choices { get; set; }

        // CurrentTopChoice equals the Choice in Choices that is the TopChoice,
        // if already tried Womans are not counted.
        public int CurrentTopChoiceIndex { get; set; } = 0;

        public string Name { get; set; }

        public Person(string name) => Name = name;

        // Returns the CurrentTopChoice based on CurrentTopChoiceIndex.
        public TPref GetCurrentTopChoice() => Choices[CurrentTopChoiceIndex];
        // Returns whether or not the given woman is the CurrentTopChoice
        public bool IsCurrentTopChoice(TPref woman)
        {
            if (woman == GetCurrentTopChoice())
                return true;
            else
                return false;
        }
    }
}
