# Graham Scan

At around the same time of the [Jarvis March](jarvis_march.md), R. L. Graham was also developing an algorithm to find the convex hull of a random set of points {{ "gs1972" | cite }}.
Unlike the Jarvis March, which is an $$\mathcal{O}(nh)$$ operation, the Graham Scan is $$\mathcal{O}(n\log(n))$$, where $$n$$ is the number of points and $$h$$ is the size fo the hull.
This means that the complexity of the Graham Scan is not output-sensitive; moreover, there are some cases where the Jarvis March is more optimal, depending on the size of the hull and the number of points to wrap.

Rather than starting at the leftmost point like the Jarvis March, the Graham scan starts at the bottom.
We then sort the distribution of points based on the angle between the bottom-most point, the origin, and each other point.
After sorting, we go through point-by-point, searching for points that are on the convex hull and throwing out any other points.
We do this by looking for counter-clockwise rotations.
If an angle between three points turns inward, the shape is obviously not convex, so we can throw that result out.
We can find whether a rotation is counter-clockwise with trigonometric functions or by using a cross-product, like so:

{% method %}
{% sample lang="jl" %}
[import:6-8, lang:"julia"](code/julia/graham.jl)
{% sample lang="hs" %}
[import:6-7, lang:"haskell"](code/haskell/grahamScan.hs)
{% sample lang="c" %}
[import:24-26, lang:"c_cpp"](code/c/graham.c)
{% sample lang="js" %}
[import:36-38, lang:"javascript"](code/javascript/graham-scan.js)
{% endmethod %}

If the output of this function is 0, the points are collinear.
If the output is positive, then the points form a counter-clockwise "left" turn.
If the output is negative, then the points form a clockwise "right" turn.
We basically do not want clockwise rotations, because this means we are at an interior angle.

<!---ADD FIGURE--->

To save memory and expensive `append()` operations, we ultimately look for points that should be on the hull and swap them with the first elements in the array.
If there are $$M$$ elements on the hull, then the first $$M$$ elements in our output random distribution of points will be the hull.
In the end, the code should look something like this:

{% method %}
{% sample lang="jl" %}
[import:10-46, lang:"julia"](code/julia/graham.jl)
{% sample lang="hs" %}
[import:9-18, lang:"haskell"](code/haskell/grahamScan.hs)
{% sample lang="c" %}
[import:65-95, lang:"c_cpp"](code/c/graham.c)
{% sample lang="js" %}
[import:1-30, lang:"javascript"](code/javascript/graham-scan.js)
{% endmethod %}

### Bibliography

{% references %} {% endreferences %}

### Example Code

{% method %}
{% sample lang="jl" %}
### Julia
[import, lang:"julia"](code/julia/graham.jl)
{% sample lang="hs" %}
### Haskell
[import, lang:"haskell"](code/haskell/grahamScan.hs)
{% sample lang="c" %}
### C
[import, lang:"c_cpp"](code/c/graham.c)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/graham-scan.js)
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
