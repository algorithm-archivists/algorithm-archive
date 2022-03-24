# Iterated Function Systems

A few quick notes before we start:

1. For this chapter, we will be following the methodology set by the [plotting chapter](../plotting/plotting.md).
That is to say that the code presented in this chapter will output another file that can be easily plotted by an external plotter.
If you like to use a plotter provided by your language of choice, please modify the code provided to do so.

2. This chapter is currently a subsection to the plotting chapter, but we may extend the algorithm archive in the future with other fractal generation methods, which would require creating a new section on fractals, in particular.
This would include a chapter with more rigorous definitions on fractals, which is largely missing from the following discussion.
Please let us know if you are interested!

In this chapter, we will show you how to make one of the most famous fractals, the Sierpinski triangle, via Iterated Function Systems (IFSs).
We will also introduce a number of interesting concepts for further exploration, such as chaos games, Hutchinson operators, and attractors.

## The Sierpinski Triangle

To begin the discussion of Iterated Function Systems (IFSs), we will first discuss what might be one of the most famous fractals currently known: the Sierpinski triangle (shown below):

<img class="center" src="res/IFS_triangle_1.png" alt="Sierpinsky Triangle Chaos Game"  style="width:100%">

This image is clearly a set of triangles embedded in a larger triangle in such a way that it can be continually cut into three identical pieces and still retain its internal structure.
This idea is known as self-similarity {{ "self-similar" | cite }}, and it is usually the first aspect of fractals to catch an audience's attention.
In fact, there are plenty of uses of fractals and their mathematical underpinnings, such as estimating the coastline of Britain {{ "mandelbrot1967long" | cite }}, identifying fingerprints {{ "jampour2010new" | cite }}, and image compression {{ "fractal-compression" | cite }}{{ "saupe1994review" | cite }}.
In many more rigorous definitions, a fractal can be described as any system that has a non-integer Hausdorff dimension {{ "3b1bfractal" | cite }}{{ "hausdorff" | cite }}{{ "gneiting2012estimators" | cite }}.
Though this is an incredibly interesting concept, the discussion of this chapter will instead focus on methods to generate fractal patterns through iterated function systems.

To start, imagine creating a triangle from three points, $$A$$, $$B$$, and $$C$$.
These points can be arbitrarily chosen, but for this conversation, we will constrict them to the vertices of an equilateral triangle, as shown below:

<img class="center" src="res/IFS_triangle_2.png" alt="Triangle Vertices"  style="width:100%">

Now let's create three separate functions that can act on a 2-dimensional space:

$$
\begin{align}
f_1(P) &= \frac{P + A}{2}\\
f_2(P) &= \frac{P + B}{2}\\
f_3(P) &= \frac{P + C}{2}\\
\end{align}
$$

Each function will read in a particular location in space (here, $$P \in \mathbb{R}^2$$) and output a new location that is the midpoint between the input location and $$A$$, $$B$$, or $$C$$ for $$f_1$$, $$f_2$$, and $$f_3$$ respectively.
The union of all of these functions (the set of all possible functions available for use) is often notated as the _Hutchinson operator_ {{ "hutchinson-operator" | cite }}{{ "hutchinson1981fractals" | cite }}, and for this case it would look like this:

$$
H(P) = \bigcup_{i=1}^3f_i(P)
$$

By iteratively using this operator, we can traverse through all possible movements in the set.
For example, let's generate 3 new points that are halfway between $$A$$ and $$B$$, $$B$$ and $$C$$, and $$A$$ and $$C$$, which will be called $$D$$, $$E$$, and $$F$$ respectively.
This is shown below:

<img class="center" src="res/IFS_triangle_3.png" alt="Triangle Midpoints"  style="width:100%">

From here, each new point ($$D$$, $$E$$, and $$F$$) will spawn 3 children, and each child will move according to one of the three possible functions in the Hutchinson operator, as shown below:

<div style="text-align:center">
<video style="width:100%" controls>
  <source src="res/IFS_triangle_vid_1.mp4" type="video/mp4">
  <img class="center" src="res/IFS_triangle_4.png" alt="First Children"  style="width:100%">
</video>
</div>

Here, all red children come from $$D$$, green children come from $$E$$ and blue children come from $$F$$.
At this stage, the children will then spawn 3 more children, each of which will move according to a different function.
Those children will then spawn more children, who act accordingly.
As this process continues on and on, we begin to see an interesting pattern form:

<div style="text-align:center">
<video style="width:100%" controls>
  <source src="res/IFS_triangle_vid_2.mp4" type="video/mp4">
  <img class="center" src="res/IFS_triangle_5.png" alt="Last children"  style="width:100%">
</video>
</div>

This is the Sierpinski triangle.
At first, it might seem like mathematical magic that a simple set of 3 functions can create such a pattern.
After all, why aren't any of the children migrating to the empty spaces in the structure?
This will require some thought, but the simplest answer is that no function within the Hutchinson operator allows for children to enter those spaces; therefore, none of the children can enter them.

## What about a square?

When I learned about how the Sierpinski triangle could be generated from 3 simple functions, I began to wonder about other shapes.
Could we create fractal squares? Hexagons? Circles?
Such shapes _seem_ like natural extensions to the triangular Hutchinson operator provided above, but there's a bit of a hitch...

First, let's take 4 points, $$A$$, $$B$$, $$C$$, and $$D$$, this time located at the four vertices of a square, like so:

<img class="center" src="res/IFS_square_1.png" alt="Sierpinsky Triangle Chaos Game"  style="width:100%">

In a similar fashion, we'll create 4 functions with $$H(P) = \bigcup_{i=1}^4f_i(P)$$, and $$P \in \mathbb{R}^2$$ such that:

$$
\begin{align}
f_1(P) &= \frac{P + A}{2}\\
f_2(P) &= \frac{P + B}{2}\\
f_3(P) &= \frac{P + C}{2}\\
f_4(P) &= \frac{P + D}{2}\\
\end{align}
$$

If we then create 5 initial points located between all the vertices and allow these points to continually spawn children like before, something peculiar happens:

<div style="text-align:center">
<video style="width:100%" controls>
  <source src="res/IFS_square_vid_1.mp4" type="video/mp4">
  <img class="center" src="res/IFS_square_2.png" alt="Hutchinson square"  style="width:100%">
</video>
</div>

We essentially see a square of squares.
What happened to the self-similar structure we were getting before?
Why isn't this more interesting?

The best answer I have for now is that some Hutchinson operators are interesting and some are not.
Still, this square is a bit more interesting than it first appears, but to see why, we need to use the Hutchinson operator in a slightly different way.

## Chaos games and attractors

Until now, our visualizations for both the Sierpinski triangle and the square have been computationally costly.
Every iteration, we generate 3 or 4 new children per child per step of the simulation.
This scales exponentially and means that we will quickly have millions of children to keep track of!
In fact, to deal with this, we developed our own method of counting through the tree to more efficiently keep track of everything, but that is a story for another day.

The question for now is whether there is a more computationally feasible way of iterating through our Hutchinson operator.

As it turns out, there is!
Rather than keeping track of every possible movement within the Hutchinson operator to draw out a shape, it's actually possible to randomly sample the function set instead through a process known as a _chaos game_ {{ "chaos-game" | cite }}{{ "chaos-game-wolf" | cite }}..
Here, instead of tracking children of children, we track a single individual that chooses randomly between the Hutchinson functions, as shown here:

{% method %}
{% sample lang="jl" %}
[import:4-17, lang:"julia"](code/julia/IFS.jl)
{% sample lang="hs" %}
[import:7-13, lang:"haskell"](code/haskell/IFS.hs)
{% sample lang="cpp" %}
[import:39-52, lang:"cpp"](code/cpp/IFS.cpp)
{% sample lang="py" %}
[import:5-12, lang:"python"](code/python/IFS.py)
{% sample lang="c" %}
[import:18-29, lang:"c"](code/c/IFS.c)
{% sample lang="lisp" %}
[import:5-14, lang:"lisp"](code/clisp/ifs.lisp)
{% sample lang="coco" %}
[import:4-16, lang:"coconut"](code/coconut/IFS.coco)
{% sample lang="rust" %}
[import:9-20, lang:"rust"](code/rust/IFS.rs)
{% sample lang="java" %}
[import:16-39, lang:"java"](code/java/IFS.java)
{% sample lang="ps1" %}
[import:2-19, lang:"powershell"](code/powershell/IFS.ps1)
{% endmethod %}

If we set the initial point to the on the equilateral triangle we saw before, we can see the Sierpinski triangle again after a few thousand iterations, as shown below:

<div style="text-align:center">
<video style="width:100%" controls>
  <source src="res/chaos_vid_1.mp4" type="video/mp4">
  <img class="center" src="res/chaos_1.png" alt="Chaos game"  style="width:100%">
</video>
</div>

Here, we are plotting 200,000 point locations in sets of 1000, and every set becomes successively more blue as the visualization continues.
At first glance, this visualization seems bewildering.
After all, it appears as if the entire triangle just magically comes into view in a few seconds.
The important thing to remember here is that each of these 200,000 dots is another location that our initial point decided to visit.

That said, there is something peculiar about the way the chaos game starts.
We are actually allowed to start the simulation *off* of the Sierpinski triangle.
As we mentioned earlier, none of the functions for the Sierpinski visualization allow children to enter the empty spaces of the triangle, so let's see what happens if we start the point off at the center of the triangle:

<div style="text-align:center">
<video style="width:100%" controls>
  <source src="res/chaos_vid_2.mp4" type="video/mp4">
  <img class="center" src="reschaos_2.png" alt="Chaos game with initial points"  style="width:100%">
</video>
</div>

Here, I have plotted the first 20 steps of the chaos game, and it is clear that the point gets closer and closer to the triangle each iteration.
Once it lands on the triangle, it can no longer escape and every movement from then on will be on the triangle.

In a sense, the wanderin point is _attracted_ to the Sierpinski triangle with this set of functions, and that is actually the case!
The truth is that the word _attractor_ is a very loaded term in the literature, but for the purposes of our discussion here, an _attractor_ is any shape defined by the iteration through Hutchinson operator functions.

So let's go back to the example with the 4 points along the square and generate the attractor via a chaos game instead of going through every branch of the Hutchinson operator.
If we do this, we get what seems to be a random distribution of points:

<img class="center" src="res/IFS_square_3.png" alt="Hutchinson square"  style="width:100%">

This kinda boggled my mind a bit when I looked at it for the first time.
What does a random distribution of points mean in this context?

Well, firstly, it's only a random distribution between the square vertices of $$A$$, $$B$$, $$C$$, and $$D$$, but nothing exists outside of these points.
This means that it's not actually a random distribution of points, but instead an attractive plane that our lone wandering point can exist happily within.

This really helped me understand how attractors present themselves in different dimensions.
The Sierpinski triangle seems like a series of lines (one-dimensional objects) in two-dimensional space, but the square is a truly two-dimensional object.
In general, this means that an attractor embedded within $$\mathbb{R}^N$$ can be any shape of dimension N or lower.

The next obvious question is whether a square can create any more interesting fractally patterns, and the answer is "yes, but only if we restrict the movement a bit."
Which brings us to another topic entirely: restricted chaos games.
Discussing restricted chaos games in more detail is a chapter in its own right, so I will forego the discussion here.
If you are interested, please let me know and I will be more than willing to add the chapter in the future!

## Video Explanation

Here is a video describing iterated function systems:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/nIIp-vo8rHg"
 frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; pic
ture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

For the code in this chapter, we have decided to write it specifically for the Chaos game, not the hutchinson animations shown at the start of the chapter.
This is because that animation is slightly tricky to create and distracts from the overall purpose of this chapter.
In addition, we have written the chaos game code to take in a set of points so that it is not hard-coded for the Sierpinski triangle and can be easily extended to other shapes like the square or restricted chaos games, as we mentioned before!

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/IFS.jl)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/IFS.hs)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/cpp/IFS.cpp)
{% sample lang="py" %}
[import, lang:"python"](code/python/IFS.py)
{% sample lang="c" %}
[import, lang:"c"](code/c/IFS.c)
{% sample lang="lisp" %}
[import, lang:"lisp"](code/clisp/ifs.lisp)
{%sample lang="coco" %}
[import, lang:"coconut"](code/coconut/IFS.coco)
{%sample lang="rust" %}
[import, lang:"rust"](code/rust/IFS.rs)
{%sample lang="java" %}
[import, lang:"java"](code/java/IFS.java)
{% sample lang="ps1" %}
[import, lang:"powershell"](code/powershell/IFS.ps1)
{% endmethod %}

### Bibliography

{% references %} {% endreferences %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/main/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

#### Images/Graphics

- The image "[IFS triangle 1](res/IFS_triangle_1.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS triangle 2](res/IFS_triangle_2.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS triangle 3](res/IFS_triangle_3.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS triangle 4](res/IFS_triangle_4.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS triangle 5](res/IFS_triangle_5.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS square 1](res/IFS_square_1.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS square 2](res/IFS_square_2.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS square 3](res/IFS_square_3.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[Chaos 1](res/chaos_1.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[Chaos 2](res/chaos_2.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[IFS triangle video 1](res/IFS_triangle_vid_1.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[IFS triangle video 2](res/IFS_triangle_vid_2.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[IFS square video 1](res/IFS_square_vid_1.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Chaos video 1](res/chaos_vid_1.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Chaos video 2](res/chaos_vid_2.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
