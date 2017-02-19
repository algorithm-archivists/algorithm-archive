# The Principles of Code

## Hindsight

I'm not going to beat around the bush here. Every single time I try something new, I fail at it. Programming was no exception. By the time I started to learn programming, I knew how to use a terminal decently well, but that was it. I remember googling, "What is the best programming language." The response was (of course) mixed. Many experts said Python was the way to go when learning programming because it was easy to use and used professionally in many areas of computer science and scientific research. Others said to go for Java, which was the most common language to learn for computer science students and thus had a large and rich archive of text to learn from. Still others said to go for C++ or FORTRAN. I was inundated with new information, and I felt like I was drowning in it. 

As a fledgling programmer, the problem for me was deciding which language was right for the projects I wanted to do. In my head, I still wanted to be an author, so I wanted to make something creative, and because my entire group of friends was into gaming at the time, I wanted a language that I could make games with. This radically changed my google search to "What language is best for making games?" It was still a bad search. Apparently, almost any language can be used to make games. Who would have guessed? 

So there I was, struggling to figure out what to do and where to go. I decided to do what any teenage boy would do in that situation: go to the bookstore and see what books were actually available for purchase at the time. Now, I think the first book I picked up really describes the desperate pit I had dug for myself. It was a book on how to make websites in HTML. Yup. I will refrain from commenting on that book for now because a much more important decision was ahead of me. There before me was an aisle with all the key players I had heard about online: C, C++, C#, Java, Python, Ruby, and so on. They were there just waiting to be chosen. I knew my time had come. I needed to make a decision then and there!

At the time, I knew the choice didn't really matter. I knew that no matter which language I learned first, most ideas would transfer from language to language. At the same time, the thought of making an uninformed decision terrified me, so I sat there in the bookstore for hours, just staring and thinking to myself. At some point, a store employee walked by and I asked if he knew anything about programming. He said he did, so I asked the only question that popped into my head at the time, "What's the difference between C, C++, and C#?" 

The employee said that C# was the newest of the three languages, but that all three were used regularly and if I wanted to learn programming, any would do. Like the experts online, he then suggested python because it was easy for beginners and I was clearly a novice. I thanked him for his time and began thumbing through the book on C#. I remember thinking, *C must be the original language. C+ must be the next generation, followed by C++, then C+++, and then they stylistically changed C++++ to C# by placing the 4 plusses in a pound sign. I guess this means that C will be a little too outdated, but C# might be a little too new. I'll go with C++.*

I was proud of my deductive reasoning, but before continuing, I would like to point out that this logic was completely flawed. There was never a C+ or a C+++ (so far as I'm aware). The fact is that I didn't know any better. My hope is that many people had similar experiences when they started programming, otherwise I am going to be really embarrassed when people start reading this book.

Suffice it to say that I learned a good deal of C++, which was a good choice in the end. It is one of the languages I use most frequently nowadays (alongside Python, CUDA, Julia, and FORTRAN), and is an essential tool in my belt. That said, I primarily do computational physics. Everyone will need a different set of tools to attack different problems. 

So. Let's address the issue of choosing a language.

As mentioned, this text does everything it can to be *language agnostic,* so the language will not matter too much when trying to learn the algorithms, themselves; however, it will matter when you try to implement them on your own. It would also be nice to formally distinguish what makes certain languages worth studying and using.

## Choosing a Language

I have a perfectly healthy habit of personifying things. I figure a good way to properly distinguish each language from each other is to list the common ones here with a small description about their typical uses. 

**These will be updated. Feel free to let me know your favorite language and why!**

#### C/C++

#### Python

#### Julia

#### Fortran

#### CUDA

#### Java

#### Haskell

## Building blocks

When it comes to programming, there are certain features that will be used again and again, so it's a good idea to mention how they work and where they are used.

### Variable and Types

For many people, this particular element of programming is straightforward. For many others, it's not. The idea was covered in basic algebra class. Variables hold values. For example, if we say 

```
x = 5
```
Then we have set the value of the variable *x* to be *5*. But here's a question that your computer has to answer: what is 5?

To us, the answer's trivial, it's a number! This is true, but there are different types of numbers. There are integers that include all counting numbers from negative to positive infinity: $$-\infty, \cdots, -1, 0, 1, \cdots, \infty$$. Then there are floating-point numbers which include everything in-between every counting number; however, that is not the end of the story. Floating-point numbers also have certain levels of precision, the numbers of 0's after the decimal point. Each of these levels of precision will require a different level of storage space on the computer, so it make sense to differentiate them based on the amount of storage they require.

But how much space does a single number take?

...

This means that we have multiple different floating-point types: float16, float32, float64, and so on. For the most part, *double precision* or float64 is good enough; however, sometimes higher precision is necessary and other times lower precision will speed up computation tremendously. 

Most languages allow you to explicitly state the type of every variable, but languages may treat these variables differently than expected. For example:

```
float x = 5
int y = 5

print(1/x)
print(1/y)
```

----
**OUTPUT**
```
0.2
0
```

Here, the same mathematical expression provided two radically different results! 

Now, obviously, variables do not need to be numbers. They can be any type you want, like strings or any common data structure, which we will cover in more detail below. Now, you might be thinking that the first output is *more correct*, but this is not exactly true. The second expression was simply a case of *integer division* and is useful in many cases. Unfortunately, it is also a notorious source of errors, which is why it's always a good idea to check the types of all variables in a program just to be sure. To be explicit:

```
1/5 != 1.0/5.0
```

It's important to keep in mind what you are trying to say and what your computer hears you saying.

Now, variables do not need to be explicitly numbers. They can be strings ("hello world!"), or any data structure you could want. These data structures will interact with each other differently, so be careful when working with them!

### Loops

Loops are weird. I'm not going to lie, it took me a super long time to figure out how and when to use them. Nowadays, I see them as essential elements to almost any program I write. 

### Conditions

These might be the most intuitive building blocks and basically flow from natural speech. If you are tired, you should go to sleep, else you should go to work! 

### Functions

Functions make sense from a mathematical point of view. $f(x) = 2x+5$ returns a value for x at any point you want. For example $f(5) = 15$, or $f(10) = 25$. Often times, the function is graphed for *every point* indicating the precise nature of how the function and variable relate to one another, but that is another matter entirely. For the most part, functions in programming work exactly the same way. They are dependent on certain variables and return something at the end. In code, the above function might look like:

```
function f(x)
    return 2*x+5
end
```

Syntactically, they are a little different, but the content is identical.
### Recursion

Simply put, recursion is the process of putting a function inside a function. Now, I know what you are thinking, "Why does this deserve a special name?" That is a very good question and one I have never been able to understand. That said, it is an incredibly good trick to have up your sleeve to speed up certain computations. 

### Classes and Structs

A while ago, one of my friends asked me the purpose of a class. At the time, I said, "It's a useful way to store a bunch of variables in one place." He then said, "But isn't it better to explicitly state each variable?" We went back and forth for some time and ended up agreeing to disagree. There was some merit to his argument. Some languages, like Julia, forego the formality of having classes at all and instead allow users to define explicit types with additional variables inside. 

## Data Structures

So this is a book about algorithms, and I have already mentioned that I have a strong desire to learn about any new algorithm that comes along, but an algorithm is only as strong as the data structures used to realize it. Earlier, we spoke of types and we tossed the word "data structure" in there. In some sense, you can think of a data structure as a stronger variable type, but it can be so much more than that. In fact, I strongly encourage you to go through the list of common data structures below and come up with your own ideas of how and why these might be used, because throughout this book we will be using and abusing every single one. It's going to be awesome!

### Trees

### Lists

### Stacks and Queues

## Code Quality

I am a computational physicist. Industry programmers give researchers a hard time for their programming capabilities, not because they cannot code, but because (for the most part) research code looks bad. Programming, like any other art form, is limited by the artist's ability to *do* art. For many computational researchers, the code is not their art. Science is. Programming is simply the brush they use to stroke the canvas of reality. Many times, they become so consumed with their painting that they ignore their tools. Now, truth be told, I have seen some pretty repulsive industry code, too, so this complaint is not exclusively biased towards researchers.

No matter what the intended goal might be, it is important to be able to read and understand the code you are writing. I know putting a comment block at the start of programs might seem like a chore, but it's not so bad, and letting everyone know up-front what the code is intended to do is incredibly useful to readers. Remember that in a few months, you will be a reader too. Take the time to properly care for your code, to clean your brush after using it. It'll pay off in the long run, I promise. 

### Compilation

### Version control

### Comments / style

Comments. Every language has a different way to introduce them. Some use the noble #, others the //, still others the ! or $ or C. Regardless of your language of choice, every single language offers you the ability to tell readers what you are intending to do with your code. This is a necessary component to programming. In fact, I would argue that it is one of the most useful part of any code. Sure, the code has to work. It's gotta compile and do something useful, but I cannot tell you how many times I have had to fix broken code that no one had looked at for years. Sometimes this is a long and laborious process, taking every ounce of my mental abilities, and other times it only takes a few minutes. What's the difference? Comments and documentation.

I remember the first time I learned to program, I found commenting tedious. I remember thinking, "Hah. This is completely useless. Any programmer worth their salt will be able to figure out what I am trying to do in no time!" Turns out, I wasn't worth my salt because after trying to figure out my code a month later, I couldn't figure out what on Earth I was trying to do. Here's the truth: when it comes to programming, you are often your own audience. Trust me, I know it seems like a good idea to keep the code short -- to go for witty one-liners that make you feel brilliant, but these make your code impossible to read. 

When researching, we keep lab notebooks. It's common practice for obvious reasons. If I forget what I was doing the other day or need to look up parameters, I check the lab notebook. If other researchers question the results of my simulations, I check my lab notebook. If I don't know what to do because I have tried everything I can possibly think of, I check my lab notebook. It's a place for inspiration, for validation, and for guidance. When programming, your code should tell the story of your program. It should reveal the hidden secrets that make your code tick. If anyone finds a bug in the code downstream, they should be able to figure out exactly what went wrong and fix it themselves. 

I guess my point is that commenting your code and keeping proper documentation is about more than just cleaning up after yourself. It is a fundamental part to the art of programming. It's like polishing your guitar on a sunny day. It's revising your poems after a long evening of writing. It's levelling your monk before the next raid. It's essential. Do not neglect it!
