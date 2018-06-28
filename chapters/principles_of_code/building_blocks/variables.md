### Variable and Types

For many people, declaring variables and types is a straightforward process.
The idea is simple: variables hold values.
We set these values with the `=` sign.
For example, if we say

```
x = 5
```
Then we have set the value of the variable $$x$$ to be $$5$$.
But here's a question that your computer has to answer: what is $$5$$?

To us, the answer's trivial, it's a number!
This is true, but there are different kinds of numbers.
There are integers that include all counting numbers from negative to positive infinity: $$-\infty, \cdots, -1, 0, 1, \cdots, \infty$$.
Then there are floating-point numbers which include everything in-between every counting number; however, that is not the end of the story.
Floating-point numbers also have certain levels of precision, the numbers of $$0$$'s after the decimal point.
For example, $$1.01$$ is more precise than $$1.0$$.
Each of these levels of precision will require a different level of storage space on the computer, so it make sense to differentiate them based on the amount of storage they require.
In the end, all of these variables will be stored as some number, $$n$$, binary bits, which are constrained to be either $$0$$ or $$1$$ and as we increase the number of bits, we can hold $$2^n$$ possible values.

This means that we have multiple different floating-point and integer types: `float16`, `int16`, `float32`, `int32`, `float64`, `int64`, and so on.
Here, the number ($$16, 32, 64,\cdots$$) at the end of the declared variable corresponds to the number of bits used to represent that number.
For the most part, *double precision* or `float64` is good enough for almost all computation; however, sometimes higher precision is necessary and other times lower precision will speed up computation tremendously.

Most languages allow you to explicitly state the type of every variable, but some languages may treat these variables differently than expected, depending on the type provided.
For example:

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
Unfortunately, it is also a notorious source of errors, which is why it's always a good idea to check the types of all variables in a program just to be sure you are doing the appropriate computation.
To be explicit:

```
1/5 != 1.0/5.0
```

It's important to keep in mind what you are trying to say and what your computer hears you saying.

Now, variables do not need to be explicitly numbers.
They can be strings (`"hello world!"`), or any data structure you could want.
These data structures will interact with each other differently and these interactions are sometimes language-dependent, so be careful when working with them!
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
Almost all laguages also allow users to define their own types in the form of `classes`, which will also be touched on later in this section.


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

