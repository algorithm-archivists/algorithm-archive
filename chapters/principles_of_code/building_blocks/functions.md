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
