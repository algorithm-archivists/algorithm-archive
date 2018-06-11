### Loops

Loops are weird. I'm not going to lie, it took me a super long time to figure out how and when to use them appropriately.
Nowadays, I see them as essential elements to almost any program I write.
There two basic loop types: `for` and `while`, which are syntactically similar and reasonably intuitive.
Let's say you want to walk out of a room with a closed door.
In code, this might look something like:

```julia
while(me.position < door.position)
    me.take_step()
end

me.open_door()
```

Like before, we assume that you are part of a type of `human`s with the functions `take_step()` and `open_door()`.
Here, the `while` loop simply keeps going until the condition is no longer met, and it's assumed that the `take_step()` function changes your `position` value.
When the condition returns `false`, we assume that you are either at the door or have run into it, so it's safe to assume you can use the `open_door()` function.

The other possible loop type is the `for` loop, which is arguable more common and iterates through a container (such as `vectors` mentioned before).
Often times, the `for` loop will look something like this:

```julia
for i = 1:10
    print(i)
end
```

In this case, we are creating a range of values between $$1$$ and $$10$$ and setting the value of $$i$$ every iteration of the loop.
In this case $$i$$ is an interable variable that steps through the range of `1:10`, and is the primary reason for using a `for` loop instead of a `while` loop for the same task.
To be clear, $$i$$ does not need to iterate through integers and could instead iterate through any number of types held in some other container.

Ultimately, loops allow programmers to repeat the same operation multiple times and are the heart most programs and simulations I have seen.


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

