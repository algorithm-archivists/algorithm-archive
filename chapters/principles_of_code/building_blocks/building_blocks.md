## Building Blocks

When it comes to programming, there are certain features that will be used again and again, so it's a good idea to mention how they work and where they are used.

### Variable and Types

For many people, this particular element of programming is straightforward. For many others, it's not. The idea was covered in basic algebra class. Variables hold values. For example, if we say 

```
x = 5
```
Then we have set the value of the variable *x* to be *5*. But here's a question that your computer has to answer: what is 5?

To us, the answer's trivial, it's a number! This is true, but there are different types of numbers. There are integers that include all counting numbers from negative to positive infinity: $$-\infty, \cdots, -1, 0, 1, \cdots, \infty$$. Then there are floating-point numbers which include everything in-between every counting number; however, that is not the end of the story. Floating-point numbers also have certain levels of precision, the numbers of 0's after the decimal point. Each of these levels of precision will require a different level of storage space on the computer, so it make sense to differentiate them based on the amount of storage they require.

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
Now, you might be thinking that the first output is *more correct*, but this is not exactly true. 
The second expression was simply a case of *integer division* and is useful in many cases. 
Unfortunately, it is also a notorious source of errors, which is why it's always a good idea to check the types of all variables in a program just to be sure. 
To be explicit:

```
1/5 != 1.0/5.0
```

It's important to keep in mind what you are trying to say and what your computer hears you saying.

Now, variables do not need to be explicitly numbers. 
They can be strings ("hello world!"), or any data structure you could want. 
These data structures will interact with each other differently, so be careful when working with them!
In addition, some of these data structures have functions unique to them that me may call upon as programmers.

For example, `vectors` are data containers that hold other data structures. 
In almost every language, there is a function that can be used to get the size of a `vector` in order to tell how many elements it holds. 
In addition, the vector also has methods to `push` elements into it.

In C++, for example, vectors often look like this:

```cpp
#include <vector>

...
// Creating a vector of integers
std::vector<int> vector_of_ints;

//Adding elements to our vector_of_ints
vector_of_ints.push_back(1);
vector_of_ints.push_back(2);
vector_of_ints.push_back(3);

// Finding size of our vector_of_ints
vector_of_ints.size();

...
```

Here, the `size()` function should return 3, because we have put in 3 elements.
We will talk more about vectors in a bit, but for now, know that certain functions are available to be used on certain types, and in most object-oriented languages, these functions are called with a simple `.`.

### Conditions

These might be the most intuitive building blocks and basically flow from natural speech.
If you are tired, you should go to sleep, or else you should go to work! 
In code, this looks like:

```julia
if (me.tired() == true)
    me.sleep()
else
    me.work()
end
```

Here, we are assuming you are a type of human with the abilities to both `sleep()` and `work()`.
Note that the condition is checking to see if the statement on the inside is `true` or `false` before continuing.
In other words, it is condensing the statement `me.tired() == true` into a simple boolean.
This is important to remember when dealing with loops later.

Though simple, conditions are essential to almost all modern code, so get used to seeing them everywhere!

### Loops

Loops are weird. I'm not going to lie, it took me a super long time to figure out how and when to use them. Nowadays, I see them as essential elements to almost any program I write. 
There two essential loop types: `for` and `while`, which are syntactically simlar and reasonably intuitive.
Let's say you want to walk out of a room with a closed door. In code, this might look something like:

```julia
while(me.position < door.position)
    me.take_step()
end

me.open_door()
```

Here again, we assume that you are part of a type of `human`s with the functions `take_step()` and `open_door()`.
Here, the `while` loop simply keeps going until the condition is no longer met.

... More to come ...


### Functions

Functions make sense from a mathematical point of view. $f(x) = 2x+5$ returns a value for x at any point you want. For example $f(5) = 15$, or $f(10) = 25$. Often times, the function is graphed for *every point* indicating the precise nature of how the function and variable relate to one another, but that is another matter entirely. For the most part, functions in programming work exactly the same way. They are dependent on certain variables and return something at the end. In code, the above function might look like:

```
function f(x)
    return 2*x+5
end
```

Syntactically, they are a little different, but the content is identical.

### Recursion

Simply put, recursion is the process of putting a function inside itself. Now, I know what you are thinking, "Why does this deserve a special name?" That is a very good question and one I have never been able to understand. That said, it is an incredibly good trick to have up your sleeve to speed up certain computations. 

### Classes and Structs

A while ago, one of my friends asked me the purpose of a class. At the time, I said, "It's a useful way to store a bunch of variables in one place." He then said, "But isn't it better to explicitly state each variable?" We went back and forth for some time and ended up agreeing to disagree. There was some merit to his argument. Some languages, like Julia, forego the formality of having classes at all and instead allow users to define explicit types with additional variables inside. 

## Data Structures

So this is a book about algorithms, and I have already mentioned that I have a strong desire to learn about any new algorithm that comes along, but an algorithm is only as strong as the data structures used to realize it. Earlier, we spoke of types and we tossed the word "data structure" in there. In some sense, you can think of a data structure as a stronger variable type, but it can be so much more than that. In fact, I strongly encourage you to go through the list of common data structures below and come up with your own ideas of how and why these might be used, because throughout this book we will be using and abusing every single one. It's going to be awesome!

### Trees

Trees are interesting data structures to say the least. It is completely filled with *nodes*, and each node has *children* nodes within it. This means that it is naturally *recursive*. It has nodes within nodes within nodes. Because of this, it is often easier to deal with trees recursively; however, sometimes it is appropriate to transform them into other data structures for later. We'll see more of these structures growing in the loamy soil of this archive. they might seem imposing at first, but you get used to them.

### Stacks and Queues

Stacks and Queues are two sides of the same coin in computer science. They are both simple data structures that hold multiple elements, but allow you to use a single element at a time. The biggest difference between the two structures is the order in which you can access the elements in the data structure.

In *stacks*, data follows *Last In, First Out* (LIFO), which basically means that whichever element you put in last will be the first element you take out. It acts exactly like a stack in real life. If you put a book on a stack of other books, the first book you will look at when sifting through the stack will be the book you just put on the stack.

In *Queues*, data follows *First In, First Out* (FIFO), which means that whichever element you put in first will be the first element you take out. Imagine a queue of people. It would be unfair if the first person in line for groceries were not the first person to receive attention once the attendant finally shows up.

For the most part, though, queues and stacks are treated the same way. There must be a way to:
1. look at the first element (`top()`)
2. to remove the first element (`pop()`)
3. to push elements onto the data structure (`push()`)

The notation for this depends on the language you are using. Queues, for example, will often use `dequeue()` instead of `pop()` and `front()` instead of `top()`. You will see the language-specific details in the source code under the algorithms in this book, so for now it's simple important to know what stacks and queues are and how to access elements held within them.
