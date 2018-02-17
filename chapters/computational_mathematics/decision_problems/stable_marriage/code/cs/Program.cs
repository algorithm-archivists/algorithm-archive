using System;
using System.Collections.Generic;
using ArcaneAlgorithmArchive.ComputationalMathematics.DecisionProblems.GaleShapley;

namespace ArcaneAlgorithmArchiveCLI
{
    class MainClass
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("GaleShapley");
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
            men[0].SetChoicesWithList(new List<Woman>() { women[3], women[2],
                women[4], women[1], women[0]});
            men[1].SetChoicesWithList(new List<Woman>() { women[0], women[2],
                women[4], women[1], women[3]});
            men[2].SetChoicesWithList(new List<Woman>() { women[2], women[1],
                women[4], women[0], women[3]});
            men[3].SetChoicesWithList(new List<Woman>() { women[2], women[3],
                women[0], women[4], women[1]});
            men[4].SetChoicesWithList(new List<Woman>() { women[4], women[0],
                women[2], women[1], women[3]});
                
            
            women[0].SetChoicesWithList(new List<Man>() { men[1], men[4],
                men[2], men[0], men[3]});
            women[1].SetChoicesWithList(new List<Man>() { men[3], men[4],
                men[2], men[0], men[1]});
            women[2].SetChoicesWithList(new List<Man>() { men[4], men[2],
                men[3], men[0], men[1]});
            women[3].SetChoicesWithList(new List<Man>() { men[1], men[4],
                men[2], men[3], men[0]});
            women[4].SetChoicesWithList(new List<Man>() { men[3], men[2],
                men[1], men[0], men[4]});

            var galeShapley = new GaleShapleyAlgorithm<Man, Woman>
            {
                Follows = men,
                Leads = women
            };
            galeShapley.RunGaleShapleyAlgorithm();
            
            foreach (var woman in women)
            {
                Console.WriteLine(woman.Name + " : " + woman?.Partner.Name);
            }
        }
    }
    
    // for GaleShapley
    public class Man : Person<Man, Woman>
    {
        public Man(string name) : base(name) { }
    }
    
    public class Woman : Person<Woman, Man>
    {
        public Woman(string name) : base(name) { }
    }
}
