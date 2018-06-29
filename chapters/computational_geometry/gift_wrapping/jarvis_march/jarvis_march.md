# Jarvis March

The first two-dimensional convex hull algorithm was originally developed by R. A. Jarvis in 1973 {{ "jm1973" | cite }}.
Though other convex hull algorithms exist, this algorithm is often called *the* gift-wrapping algorithm.

The idea behind this algorithm is simple.
If we start with a random distribution of points, we can find the convex hull by first starting with the left-most point and using the origin to calculate an angle between every other point in the simulation.
As a note, the "angle" can be roughly approximated with a cross-product or a dot product, which is common for some implementations here.
Whichever point has the largest interior angle is chosen as the next point in the convex hull and we draw a line between the two points.
From there, we use the two known points to again calculate the angle between all other points in the simulation.
We then choose the point with the largest interior angle and move the simulation forward.
We keep repeating this process until we have returned to our original point.
The set of points chosen in this simulation will be the convex hull.

As we might expect, this algorithm is not incredibly efficient and has a runtime of $$\mathcal{O}(nh)$$, where $$n$$ is the number of points and $$h$$ is the size of the hull.
As a note, the Jarvis March can be generalized to higher dimensions.
Since this algorithm, there have been many other algorithms that have advanced the field of two-dimensional gift-wrapping forward, including the Graham Scan and Chan's Algorithm, which will be discussed in due time.

### Bibliography

{% references %} {% endreferences %}

### Example Code

{% method %}
{% sample lang="cs" %}
### C# #
JarvisMarch.cs
[import, lang="csharp"](code/cs/JarvisMarch.cs)
Program.cs
[import, lang="csharp"](code/cs/Program.cs)
{% sample lang="jl" %}
### Julia
[import, lang:"julia"](code/julia/jarvis.jl)
{% sample lang="hs" %}
### Haskell
[import, lang:"haskell"](code/haskell/jarvisMarch.hs)
{% sample lang="c" %}
### C
[import, lang:"c_cpp"](code/c/jarvis_march.c)
{% sample lang="js" %}
### JavaScript
[import, lang:"javascript"](code/javascript/jarvis-march.js)
{% endmethod %}

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
