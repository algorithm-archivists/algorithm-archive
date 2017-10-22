<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
$$ 
\newcommand{\d}{\mathrm{d}}
\newcommand{\bff}{\boldsymbol{f}}
\newcommand{\bfg}{\boldsymbol{g}}
\newcommand{\bfp}{\boldsymbol{p}}
\newcommand{\bfq}{\boldsymbol{q}}
\newcommand{\bfx}{\boldsymbol{x}}
\newcommand{\bfu}{\boldsymbol{u}}
\newcommand{\bfv}{\boldsymbol{v}}
\newcommand{\bfA}{\boldsymbol{A}}
\newcommand{\bfB}{\boldsymbol{B}}
\newcommand{\bfC}{\boldsymbol{C}}
\newcommand{\bfM}{\boldsymbol{M}}
\newcommand{\bfJ}{\boldsymbol{J}}
\newcommand{\bfR}{\boldsymbol{R}}
\newcommand{\bfT}{\boldsymbol{T}}
\newcommand{\bfomega}{\boldsymbol{\omega}}
\newcommand{\bftau}{\boldsymbol{\tau}}
$$

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

