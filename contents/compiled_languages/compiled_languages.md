# Compiled Languages

Programming is hard because we need to write instructions for our computer.
The computer then reads this set of instructions and executes them accordingly.
That's the story, at least.
In principle, there is more to it: *compilation*.
Compilation is the act of translating what we write into something the computer can read.
This is done with a *compiler*.
In this section, we will talk about all the compiled languages and discuss how to deal with different compilers and compiling options.

The first  book I read in C++ had a chapter devoted to running C++ code for the first time on Windows, Mac, and Linux devices.
If I'm being honest, this was by far the most confusing part of programming for me.
I could grasp the computing concepts, but getting the computer to run my code seemed like an impossible chore.
I blame this somewhat on the text I was reading at the time, as it strongly encouraged the usage of Integrated Development Environments (IDE) that I couldn't figure out.
IDE's are coding environments that pull up a GUI text editor to write and run code in.
This is obviously an optimal way to program; however, I tend to dislike GUI-based solutions when there is a command-line option available.
It wasn't until I saw a compilation command in my terminal that it started to make sense:

```
g++ hello_world.cpp -o output_executable
./output_executable
```

To me, this was elegant and straightforward.
We compile code with a compiler, `g++`.
We supply the code, `hello_world.cpp`.
We tell the compiler where to output the code, `-o output_executable`.
We execute the code, `./output_executable`.
Done.

Again, there is nothing wrong with using an IDE, they are incredibly useful for large software projects and often have debugging in-built, which is a huge benefit!
I just find it easier to avoid GUI's whenever possible.

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
