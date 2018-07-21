# Bogo Sort
Look, Bogo Sort doesn't really sort anything out.
In fact, it should never be used in practice for any reason I can think of and really only serves as a joke in the programming community.
As far as jokes go, though, this one's pretty good.

So, here's how it goes:
imagine you have an array of $$n$$ elements that you want sorted.
One way to do it is to shuffle the array at random and hope that all the elements will be magically in order after shuffling.
If they are not in order, just shuffle everything again.
And then again. And again.
In the best case, this algorithm runs with a complexity of $$\Omega(n)$$, and in the worst, $$\mathcal{O}(\infty)$$.

In code, it looks something like this:

{% method %}
{% sample lang="jl" %}
[import:10-14, lang:"julia"](code/julia/bogo.jl)
{% sample lang="cs" %}
[import:9-15, lang:"csharp"](code/csharp/BogoSort.cs)
{% sample lang="clj" %}
[import:7-11, lang:"clojure"](code/clojure/bogo.clj)
{% sample lang="c" %}
[import:25-29, lang:"c_cpp"](code/c/bogo_sort.c)
{% sample lang="java" %}
[import:2-6, lang:"java"](code/java/bogo.java)
{% sample lang="js" %}
[import:11-15, lang:"javascript"](code/javascript/bogo.js)
{% sample lang="py" %}
[import:10-12, lang:"python"](code/python/bogo.py)
{% sample lang="hs" %}
[import:17-20, lang:"haskell"](code/haskell/bogoSort.hs)
{% sample lang="m" %}
[import:21-28, lang:"matlab"](code/matlab/bogosort.m)
{% sample lang="cpp" %}
[import:33-38, lang:"c_cpp"](code/c++/bogosort.cpp)
{% sample lang="rs" %}
[import:16-20, lang:"rust"](code/rust/bogosort.rs)
{% sample lang="swift" %}
[import:32-39, lang:"swift"](code/swift/bogosort.swift)
{% endmethod %}

That's it.
Ship it!
We are done here!

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/bogo.jl)
{% sample lang="cs" %}
BogoSort.cs
[import, lang:"csharp"](code/csharp/BogoSort.cs)
Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
{% sample lang="clj" %}
[import, lang:"clojure"](code/clojure/bogo.clj)
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/bogo_sort.c)
{% sample lang="java" %}
[import, lang:"java"](code/java/bogo.java)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/bogo.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/bogo.py)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bogoSort.hs)
{% sample lang="m" %}
[import, lang:"matlab"](code/matlab/bogosort.m)
{% sample lang="cpp" %}
[import, lang:"c_cpp"](code/c++/bogosort.cpp)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/bogosort.rs)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/bogosort.swift)
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
