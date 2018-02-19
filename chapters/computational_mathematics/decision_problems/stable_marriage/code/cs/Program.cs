﻿// submitted by Julian Schacher (jspp) with great help by gustorn and Marius Becker
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
            men[0].Choices = new List<Woman>() { women[3], women[2],
                women[4], women[1], women[0]};
            men[1].Choices = new List<Woman>() { women[0], women[2],
                women[4], women[1], women[3]};
            men[2].Choices = new List<Woman>() { women[2], women[1],
                women[4], women[0], women[3]};
            men[3].Choices = new List<Woman>() { women[2], women[3],
                women[0], women[4], women[1]};
            men[4].Choices = new List<Woman>() { women[4], women[0],
                women[2], women[1], women[3]};
                
            
            women[0].Choices = new List<Man>() { men[1], men[4],
                men[2], men[0], men[3]};
            women[1].Choices = new List<Man>() { men[3], men[4],
                men[2], men[0], men[1]};
            women[2].Choices = new List<Man>() { men[4], men[2],
                men[3], men[0], men[1]};
            women[3].Choices = new List<Man>() { men[1], men[4],
                men[2], men[3], men[0]};
            women[4].Choices = new List<Man>() { men[3], men[2],
                men[1], men[0], men[4]};

            GaleShapleyAlgorithm<Woman, Man>.RunGaleShapleyAlgorithm(women, men);
            
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
