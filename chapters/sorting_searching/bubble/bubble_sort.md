# Bubble Sort
When it comes to sorting algorithms, Bubble Sort is usually the first that comes to mind.
Though it might not be the fastest tool in the shed, it's definitely straightforward to implement and is often the first sorting method new programmers think of when trying to implement a sorting method on their own.

Here's how it works: we go through each element in our vector and check to see if it is larger than the element to it's right.
If it is, we swap the elements and then move to the next element.
In this way, we sweep through the array $$n$$ times for each element and continually swap any two adjacent elements that are improperly ordered.
This means that we need to go through the vector $$\mathcal{O}(n^2)$$ times with code similar to the following:

{% method %}
{% sample lang="jl" %}
[import:1-10, lang:"julia"](code/julia/bubble.jl)
{% sample lang="cs" %}
[import:9-27, lang:"csharp"](code/cs/BubbleSort.cs)
{% sample lang="c" %}
[import:3-21, lang:"c_cpp"](code/c/bubble_sort.c)
{% sample lang="java" %}
[import:2-12, lang:"java"](code/java/bubble.java)
{% sample lang="js" %}
[import:1-11, lang:"javascript"](code/js/bubble.js)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bubbleSort.hs)
{% sample lang="cpp" %}
[import, lang:"c_cpp"](code/c++/bubblesort.cpp)
{% sample lang="rs" %}
[import:6-19, lang:"rust"](code/rust/bubble_sort.rs)
{% sample lang="d" %}
[import:3-18, lang:"d"](code/d/bubble_sort.d)
{% sample lang="go" %}
[import:7-21, lang:"go"](code/go/bubbleSort.go)
{% sample lang="racket" %}
[import:5-19, lang:"racket"](code/racket/bubbleSort.rkt)
{% endmethod %}

... And that's it for the simplest bubble sort method.
Now, as you might imagine, computer scientists have optimized this to the fiery lakes of Michigan and back, so we'll come back to this in the future and talk about how to optimize it.
For now, it's fine to just bask in the simplicity that is bubble sort.
Trust me, there are plenty of more complicated algorithms that do precisely the same thing, only much, much better (for most cases).


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
