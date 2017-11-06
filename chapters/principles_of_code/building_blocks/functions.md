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

### Functions

Functions make sense from a mathematical point of view. 
$$f(x) = 2x+5$$ returns a value for x at any point you want. 
For example $$f(5) = 15$$, or $$f(10) = 25$$.
Often times, the function is graphed for *every point* indicating the precise nature of how the function and variable relate to one another, but that is another matter entirely. 
For the most part, functions in programming work exactly the same way. 
They are dependent on certain variables and return something at the end. 
In code, the above function might look like:

```
function f(x)
    return 2*x+5
end
```

Syntactically, they are a little different, but the content is identical.
That said, it's not obvious how functions help when writing code at this point; however, this will become incredibly obvious once you see them in action.
Basically, functions allow programmers to create reusable operations that can be called at any point in the code.
In a sense, functions make understanding code much, much easier.

### Recursion

Simply put, recursion is the process of putting a function inside itself. 
Now, I know what you are thinking, "Why does this deserve a special name?" 
That is a very good question and one that tooke me a while to understand. 
Let's take a simple example:

```julia
function f(x)
    x = x + 1
    f(x)
end
```

Every time `f(x)` is called, it calls `f(x)`, which then calls `f(x)` again and again and again...
Basically, we just replicated a `while` loop!
The problem here is that *we forgot a stop condition!*
This means that our function will run forever, constantly incrementing the value of x for all eternity!

There are many possible solutions, one of which looks like this:

```julia
function f(x, cutoff)
    if (x < cutoff)
        x = x + 1
        f(x)
    end
end
```

In the end, no matter how you choose to use recursion, you need to think carefully about what you want the code to do.
For example, in the case shown here, recursion is definitely not the most straightforward way to increment the value of $$x$$ by $$1$$.
In some cases, though, recursion not only makes the code easier to use and understand, but it might also be the only way to solve a provided problem.
In addition, the proper use of recursion can speed up certain code tremendously.

I guess the main point is this: *recursion is powerful, but with great power comes great resposibility!*
