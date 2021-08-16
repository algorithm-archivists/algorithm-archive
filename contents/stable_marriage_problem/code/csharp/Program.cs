// submitted by Julian Schacher (jspp) with great help by gustorn and Marius Becker
using System;
using System.Collections.Generic;

namespace StableMarriageProblem
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("GaleShapleyAlgorithm");
            // Using men and women as an example.
            var men = new List<Man>()
            {
                new Man("A"),
                new Man("B"),
                new Man("C"),
                new Man("D"),
                new Man("E")
            };
            var women = new List<Woman>()
            {
                new Woman("F"),
                new Woman("G"),
                new Woman("H"),
                new Woman("I"),
                new Woman("J"),
            };

            var random = new Random();

            foreach (var man in men)
            {
                man.Choices = new List<Woman>(women).Shuffle(random);
                Console.WriteLine(man.Name + ":");
                foreach (var choice in man.Choices)
                    Console.Write(choice.Name);
                Console.WriteLine();
            }
            foreach (var woman in women)
            {
                woman.Choices = new List<Man>(men).Shuffle(random);
                Console.WriteLine(woman.Name + ":");
                foreach (var choice in woman.Choices)
                    Console.Write(choice.Name);
                Console.WriteLine();
            }

            GaleShapleyAlgorithm<Woman, Man>.RunGaleShapleyAlgorithm(women, men);

            foreach (var woman in women)
            {
                Console.WriteLine(woman.Name + " : " + woman?.Partner.Name);
            }
        }
    }

    public class Man : Person<Man, Woman>
    {
        public Man(string name) : base(name) { }
    }

    public class Woman : Person<Woman, Man>
    {
        public Woman(string name) : base(name) { }
    }
}
