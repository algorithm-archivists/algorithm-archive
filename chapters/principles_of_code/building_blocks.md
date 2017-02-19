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

