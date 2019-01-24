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

## Example Code

{% method %}
{% sample lang="cs" %}
##### JarvisMarch.cs
[import, lang="csharp"](code/csharp/JarvisMarch.cs)
##### Program.cs
[import, lang="csharp"](code/csharp/Program.cs)
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/jarvis.jl)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/jarvisMarch.hs)
{% sample lang="c" %}
[import, lang:"c"](code/c/jarvis_march.c)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/jarvis-march.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/jarvis_march.py)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/jarvis_march.cpp)
{% sample lang="lisp" %}
[import, lang:"lisp"](code/clisp/jarvis-march.lisp)
{% sample lang="java" %}
[import, lang:"java"](code/java/JarvisMarch.java)
{% sample lang="go" %}
[import, lang:"go"](code/golang/jarvis.go)
{% endmethod %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
