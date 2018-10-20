# The Stable Marriage Problem
Imagine you have two groups, each of size $$n$$.
Each individual within a group has an internal ranking associated with all members of the opposing group.
The *Stable Matching Problem* attempts to unite both groups into stable pairs.
In this case, a set of pairs is considered stable if there are no pairs that like each other more than their current partners.
This doesn't mean that everyone gets their top choices, but if an individual prefers someone else who also prefers them back, the set of pairs is not stable.

Now, this is often told as a story.
One group is male, the other is female, and everyone gets married, hence the name the *Stable Marriage Problem*.
This problem is solved by the Gale-Shapley algorithm, which can be simply described as follows:

1. All the men propose to their top choice of women.
2. The women become tentatively engaged to their top choice of the men who have proposed to them.
3. All rejected men propose to their next choice, and the women again select whichever man they prefer, possibly rejecting the one they were already engaged to.

This process continues until all individuals are paired, which means that this algorithm guarantees stable matching and also has a $$\mathcal{O}(n^2)$$ runtime.
To be clear, even though this algorithm seems conceptually simple, it is rather tricky to implement correctly.
I do not at all claim that the code provided here is efficient and we will definitely be coming back to this problem in the future when we have more tools under our belt.
I am incredibly interested to see what you guys do and how you implement the algorithm.

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/stable_marriage.jl)
{% sample lang="py" %}
[import, lang:"python"](code/python/stable_marriage.py)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/stableMarriage.hs)
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/stable_marriage.c)
{% sample lang="cpp" %}
[import, lang:"c_cpp"](code/c++/stable_marriage.cpp)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/stable-marriage.js)
{% sample lang="cs" %}
##### GaleShapleyAlgorithm.cs
[import, lang:"csharp"](code/csharp/GaleShapleyAlgorithm.cs)
##### Person.cs
[import, lang:"csharp"](code/csharp/Person.cs)
##### Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
##### ListExtensions.cs
[import, lang:"csharp"](code/csharp/ListExtensions.cs)
{% sample lang="java" %}
[import, lang:"java"](code/java/stable-marriage.java)
{% sample lang="php" %}
[import, lang:"php"](code/php/stable_marriage.php)
{% sample lang="scala" %}
[import, lang:"scala"](code/scala/stable_marriage.scala)
{% endmethod %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
