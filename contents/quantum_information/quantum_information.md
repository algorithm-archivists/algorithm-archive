# Quantum Information

Quantum information theory is... intense.
It requires a strong and fundamental understanding of classical information theory and quantum mechanics.
It is not obvious in any way and deserves many textbooks on it's own.
In fact, there are numerous textbooks on the subject already.
The purpose of this section isn't to outdo any of that fundamental knowledge.
Instead, we will attempt to distill the knowledge into a short, intuitive summary, with the hopes of helping people to understand more about the topic and pursue it further on their own.

At the time of writing, true quantum computers do not exist.
We do have some systems that are able to simulate qubits, they are not truly universal quantum computers.
The closest market-ready system we currently have is D-WAVE, which boasts an impressive 128 qubits!

There are many places to start an introduction to quantum information theory, so we'll go through it one step at a time:

1. **Quantum bitlogic:** what is a qubit and how is it different than a classical bit?
2. **Quantum gates and quantum circuits:** How do you fundamentally build a quantum algorithm?
3. **Quantum computers in the wild:** Current experimental techniques to create a quantum computer and what makes them ill-suited as real quantum computers
4. **A survey of current quantum algorithms:** There are a number of algorithms that promise fantastic advantages when performed on quantum computers and should really shake up the industry when they are finally experimentally realized.

As a note, item 3 might seem out of place for a book on algorithms, and I would tend to agree; however, at this point there is a phenomenal amount of research being done to realize the first truly quantum computer and there are a number of potential systems that could work for this purpose.
These systems will change how we think about and interface with quantum computation in the future and it is important to discuss where the field might be heading and when we can expect quantum computers at home.

Now, there are not too many languages that can compile quantum code.
A while ago, we tried to make a quantum circuit compiler, which was modeled after the SPICE circuit simulator, but this was far from a computer language.
At this point in time, it is impossible to tell what quantum computing languages will look like when we finally have a truly quantum machine, so for the time being, we will not ask for community code for the chapters related to quantum information.

basically, it's hard to imagine how to would adequately implement Shor's algorithm in C.
As always, this section will be updated as we add more algorithms to the list.


## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
