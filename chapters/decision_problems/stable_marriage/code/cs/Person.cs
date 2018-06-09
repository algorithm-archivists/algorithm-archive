// submitted by Julian Schacher (jspp) with great help by gustorn and Marius Becker
using System.Collections.Generic;

namespace StableMarriageProblem
{
    public class Person<TSelf, TPref>
        where TSelf : Person<TSelf, TPref>
        where TPref : Person<TPref, TSelf>
    {
        public string Name { get; set; }
        public TPref Partner { get; set; }
        public IList<TPref> Choices { get; set; }
        // CurrentTopChoice equals the Choice in Choices that is the TopChoice,
        // if already tried women are not counted.
        public int CurrentTopChoiceIndex { get; set; } = 0;

        public Person(string name) => Name = name;

        public void ProposeToNext()
        {
            var interest = GetNextTopChoice();

            // If the interest has no partner or prefers this person,
            // change interest's partner to this person.
            if (interest.Partner == null ||
                interest.Choices.IndexOf(this as TSelf) < interest.Choices.IndexOf(interest.Partner))
            {
                // Should the interest already have a partner, set the partner
                // of the interest's partner to null.
                if (interest.Partner != null)
                    interest.Partner.Partner = null;
                interest.Partner = this as TSelf;
                Partner = interest;
            }
        }

        private TPref GetNextTopChoice() => Choices[CurrentTopChoiceIndex++];
    }
}
