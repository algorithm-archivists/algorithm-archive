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
[import:9-27, lang:"csharp"](code/csharp/BubbleSort.cs)
{% sample lang="c" %}
[import:10-20, lang:"c_cpp"](code/c/bubble_sort.c)
{% sample lang="java" %}
[import:2-12, lang:"java"](code/java/bubble.java)
{% sample lang="js" %}
[import:1-11, lang:"javascript"](code/javascript/bubble.js)
{% sample lang="py" %}
[import:4-9, lang:"python"](code/python/bubblesort.py)
{% sample lang="m" %}
[import:11-23, lang:"matlab"](code/matlab/bubblesort.m)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bubbleSort.hs)
{% sample lang="cpp" %}
[import:13-23, lang:"c_cpp"](code/c++/bubblesort.cpp)
{% sample lang="rs" %}
[import:6-16, lang:"rust"](code/rust/bubble_sort.rs)
{% sample lang="d" %}
[import:3-18, lang:"d"](code/d/bubble_sort.d)
{% sample lang="go" %}
[import:7-21, lang:"golang"](code/go/bubbleSort.go)
{% sample lang="racket" %}
[import:6-19, lang:"racket"](code/racket/bubbleSort.rkt)
{% sample lang="swift" %}
[import:1-13, lang:"swift"](code/swift/bubblesort.swift)
{% sample lang="ti83b" %}
[import:2-13, lang:"ti-83_basic"](code/ti83basic/BUBLSORT.txt)
{% sample lang="ruby" %}
[import:3-13, lang:"ruby"](code/ruby/bubble.rb)
{% sample lang="crystal" %}
[import:1-11, lang:"crystal"](code/crystal/bubble.cr)
{% endmethod %}

... And that's it for the simplest bubble sort method.
Now, as you might imagine, computer scientists have optimized this to the fiery lakes of Michigan and back, so we'll come back to this in the future and talk about how to optimize it.
For now, it's fine to just bask in the simplicity that is bubble sort.
Trust me, there are plenty of more complicated algorithms that do precisely the same thing, only much, much better (for most cases).

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/bubble.jl)
{% sample lang="cs" %}
BubbleSort.cs
[import, lang:"csharp"](code/csharp/BubbleSort.cs)
Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/bubble_sort.c)
{% sample lang="java" %}
[import, lang:"java"](code/java/bubble.java)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/bubble.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/bubblesort.py)
{% sample lang="m" %}
[import, lang:"matlab"](code/matlab/bubblesort.m)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bubbleSort.hs)
{% sample lang="cpp" %}
[import, lang:"c_cpp"](code/c++/bubblesort.cpp)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/bubble_sort.rs)
{% sample lang="d" %}
[import, lang:"d"](code/d/bubble_sort.d)
{% sample lang="go" %}
[import, lang:"golang"](code/go/bubbleSort.go)
{% sample lang="racket" %}
[import, lang:"racket"](code/racket/bubbleSort.rkt)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/bubblesort.swift)
{% sample lang="ti83b" %}
[import, lang:"ti-83_basic"](code/ti83basic/BUBLSORT.txt)
{% sample lang="ruby" %}
[import, lang:ruby"](code/ruby/bubble.rb)
{% sample lang="crystal" %}
[import, lang:"crystal"](code/crystal/bubble.cr)
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
