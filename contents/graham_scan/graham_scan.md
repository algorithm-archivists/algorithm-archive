# Graham Scan

At around the same time of the [Jarvis March](../jarvis_march/jarvis_march.md), R. L. Graham was also developing an algorithm to find the convex hull of a random set of points {{ "gs1972" | cite }}.
Unlike the Jarvis March, which is an $$\mathcal{O}(nh)$$ operation, the Graham Scan is $$\mathcal{O}(n\log(n))$$, where $$n$$ is the number of points and $$h$$ is the size for the hull.
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
[import:24-26, lang:"c"](code/c/graham.c)
{% sample lang="js" %}
[import:36-38, lang:"javascript"](code/javascript/graham-scan.js)
{% sample lang="py" %}
[import:4-6, lang:"python"](code/python/grahamScan.py)
{% sample lang="go" %}
[import:13-15, lang:"go"](code/golang/graham.go)
{% sample lang="java" %}
[import:27-29, lang:"java"](code/java/GrahamScan.java)
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
[import:10-45, lang:"julia"](code/julia/graham.jl)
{% sample lang="hs" %}
[import:9-18, lang:"haskell"](code/haskell/grahamScan.hs)
{% sample lang="c" %}
[import:65-95, lang:"c"](code/c/graham.c)
{% sample lang="js" %}
[import:1-30, lang:"javascript"](code/javascript/graham-scan.js)
{% sample lang="py" %}
[import:14-27, lang:"python"](code/python/grahamScan.py)
{% sample lang="go" %}
[import:21-42, lang:"go"](code/golang/graham.go)
{% sample lang="java" %}
[import:35-70, lang:"java"](code/java/GrahamScan.java)
{% endmethod %}

### Bibliography

{% references %} {% endreferences %}

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/graham.jl)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/grahamScan.hs)
{% sample lang="c" %}
[import, lang:"c"](code/c/graham.c)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/graham-scan.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/grahamScan.py)
{% sample lang="go" %}
[import, lang:"go"](code/golang/graham.go)
{% sample lang="java" %}
[import, lang:"java"](code/java/GrahamScan.java)
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
