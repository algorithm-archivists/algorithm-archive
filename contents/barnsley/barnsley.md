# The Barnsley Fern

At the end of the chapter on [Iterated Function Systems](../IFS/IFS.md), we introduced two separate attractors: the Sierpinski triangle, and a uniform two-dimensional square, shown below with their corresponding Hutchinson operator.

| Hutchinson Operator | Attractor |
| ------------------- | --------- |
| $$\begin{align} f_1(P) &= \frac{P+A}{2} \\  f_2(P) &= \frac{P+B}{2} \\  f_3(P) &= \frac{P+C}{2} \end{align}$$ | <img class="center" src="../IFS/res/IFS_triangle_1.png" alt="Sierpinsky Triangle Chaos Game"  style="width:100%"> |
| $$\begin{align} f_1(P) &= \frac{P+A}{2} \\  f_2(P) &= \frac{P+B}{2} \\  f_3(P) &= \frac{P+C}{2} \\ f_4(P) &= \frac{P+D}{2} \end{align}$$ | <img class="center" src="../IFS/res/IFS_square_3.png" alt="Square Chaos Game"  style="width:100%"> |

As a reminder, the Hutchinson operator is a set of functions that act on a point in space, $$P$$, and return another another point at a new location.
These functions are meant to be used over and over again in some fashion, and as you continually iterate through them, some shape will eventually be drawn.
This shape is known as an attractor, and the entire system is called an *iterated function system* due to the iterative nature of drawing the attractor.

In these cases, each function will move the point to be halfway between its original position and the position of $$A$$, $$B$$, $$C$$, and $$D$$ for $$f_1$$, $$f_2$$, $$f_3$$, and $$f_4$$, respectively.
Even though $$f_1$$, $$f_2$$, and $$f_3$$ are the same for both attractors, the addition of $$f_4$$ drastically changes the final result!
It is surprising that two seemingly identical sets of functions can look so different in the end, and this leads us to a somewhat challenging question: given a set of functions, is there any way to predict what the attractor will be without iterating through the functions?

In general, the answer is no.
You *must* sample the function set in some fashion to get find the resulting attractor.

This feels somewhat unsettling to me.
After all, each individual function is simple, so why is the result so difficult to predict?
In this chapter, I hope to provide a slightly more satisfying answer by introducing another iterated function system with beautiful attractor, known as the Barnsley fern {{ "barnsley2014fractals" | cite }}:

| Hutchinson Operator | Attractor |
| ------------------- | --------- |
| $$\begin{align} f_1(P) &= \begin{bmatrix} 0 &0 \\ 0 &0.16 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0 \end{bmatrix} \\ f_2(P) &= \begin{bmatrix} 0.85 &0.04 \\ -0.04 &0.85 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix} \\ f_3(P) &= \begin{bmatrix} 0.2 &-0.26 \\ 0.23 &0.22 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix} \\ f_4(P) &= \begin{bmatrix} -0.15 &0.28 \\ 0.26 &0.24 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0.44 \end{bmatrix} \end{align}$$ | <img class="center" src="res/full_fern.png" alt="Barnsley Chaos Game"  style="width:100%"> |

At first glance, this set of functions looks like an incomprehensible mess of magic numbers to create a specific result, and in a sense, that is precisely correct.
That said, we will go through each function and explain how it works, while also providing a simple chaos game implementation in code.
By the end of this chapter, we do not hope to provide a general strategy for understanding all iterated function systems, but we hope to at least make this one set of functions a bit more understandable.

## Individual affine transforms

The first thing to note about the Barnsley set of functions is that each one is an [affine transformation](../affine_transformations/affine_transformations.md).
Though it is not a hard rule, most iterated function systems use affine transforms, so this notation is common.
In fact, the Sierpinski operators can also be written in an affine form:

| Non-affine | Affine |
| ---------- | ------ |
| $$\begin{align} f_1(P) &= \frac{P+A}{2} \\  f_2(P) &= \frac{P+B}{2} \\  f_3(P) &= \frac{P+C}{2} \end{align}$$ | $$\begin{align} f_1(P) &= \begin{bmatrix} 0.5 &0 \\ 0 &0.5 \end{bmatrix}P + \frac{A}{2} \\ f_2(P) &= \begin{bmatrix} 0.5 &0 \\ 0 &0.5 \end{bmatrix}P + \frac{B}{2} \\ f_3(P) &= \begin{bmatrix} 0.5 &0 \\ 0 &0.5 \end{bmatrix}P + \frac{C}{2} \end{align}$$ |

The affine variant performs the same operation by scaling the $$x$$ and $$y$$ component of $$P$$ by $$0.5$$ and then adding half of either $$A$$, $$B$$, or $$C$$ for $$f_1$$, $$f_2$$, or $$f_3$$, respectively.
Each of these transforms involves some linear component (scaling or shearing) with an additional translation.

As an important side-note: in both the Barnsley and Sierpinski function systems, the coefficients of the transformation matrix are all less than 1.
This property is known as *contractivity*, and an iterated function system can only have an attractor if the system is contractive.
Upon reflection, this makes sense.
If the matrix elements were greater than 1, the point could tend towards infinity after successive iterations of the function.

Now let's hop into disecting the Barnsley fern by seeing how each transform affects a random distribution of points:

| Function | Operation |
| -------- | --------- |
| $$f_1(P) = \begin{bmatrix} 0 &0 \\ 0 &0.16 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0 \end{bmatrix}$$ <p> This operation moves every point to a single line. | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_rnd_0.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_2(P) = \begin{bmatrix} 0.85 &0.04 \\ -0.04 &0.85 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ <p> This operation moves every point up and to the right. | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_rnd_1.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_3(P) = \begin{bmatrix} 0.2 &-0.26 \\ 0.23 &0.22 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ <p> This operation rotates every point to the left. | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_rnd_2.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_4(P) = \begin{bmatrix} -0.15 &0.28 \\ 0.26 &0.24 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0.44 \end{bmatrix}$$ <p> This operation flips every point and rotates to the right.| <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_rnd_3.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

At this stage, it *might* be clear what is going on, but it's not exactly obvious.
Essentially, each operation corresponds to another part of the fern:

* $$f_1$$ creates the stem.
* $$f_2$$ creates successively smaller ferns moving up and to the right.
* $$f_3$$ creates the leaves on the right.
* $$f_4$$ creates the leaves on the left.

The easiest way to make sense of this is to show the operations on the Barnsley fern, itself, instead of a random distribution of points.

| Function | Operation |
| -------- | --------- |
| $$f_1(P) = \begin{bmatrix} 0 &0 \\ 0 &0.16 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0 \end{bmatrix}$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_fern_0.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_2(P) = \begin{bmatrix} 0.85 &0.04 \\ -0.04 &0.85 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_fern_1.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_3(P) = \begin{bmatrix} 0.2 &-0.26 \\ 0.23 &0.22 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_fern_2.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_4(P) = \begin{bmatrix} -0.15 &0.28 \\ 0.26 &0.24 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0.44 \end{bmatrix}$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/affine_fern_3.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

Here, the self-similar nature of the fern becomes apparent.
Each operation is effectively moving a point on one part of the fern to a point on another part of the fern.

In the final construction, it is clear that fewer points are necessary on some parts than others.
The stem, for example, does not need many points at all.
Meanwhile, the bulk of the fern seems to be generated by $$f_2$$, so we probably want the majority of the points to choose that function when iterating through he set.
To account for this, each function is also given a probability of being chosen:

| Function | Probability |
| -------- | ----------- |
| $$f_1(P) = \begin{bmatrix} 0 &0 \\ 0 &0.16 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0 \end{bmatrix}$$ | 0.01 |
| $$f_2(P) = \begin{bmatrix} 0.85 &0.04 \\ -0.04 &0.85 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ | 0.85 |
| $$f_3(P) = \begin{bmatrix} 0.2 &-0.26 \\ 0.23 &0.22 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ | 0.07 |
| $$f_4(P) = \begin{bmatrix} -0.15 &0.28 \\ 0.26 &0.24 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0.44 \end{bmatrix}$$ | 0.07 |

## Playing around a bit...

One big advantage of using affine transformations to construct an attractor is that mathematicians and programmers can leverage their knowledge of how these transformations work to also modify the resulting image.
Here are a few examples of ferns that can be generated by modifying constituent functions:

| Function | Operation |
| -------- | --------- |
| $$f_1(P) = \begin{bmatrix} \tau &0 \\ 0 &0.16 \end{bmatrix}P + \begin{bmatrix} 0 \\ 0 \end{bmatrix}$$ <p> where $$-0.5 < \tau < 0.5 $$ <p> Turning stems to leaves | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/fern_twiddle_0.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_2(P) = \begin{bmatrix} 0.85 & \tau \\ -0.04 &0.85 \end{bmatrix}P + \begin{bmatrix} 0 \\ 1.6 \end{bmatrix}$$ <p> where $$ -0.01 < \tau < 0.09 $$ <p> Changing fern tilt | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/fern_twiddle_1.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_3(P) = \begin{bmatrix} 0.2 &-0.26 \\ 0.23 &0.22 \end{bmatrix}P + \begin{bmatrix} \tau \\ 1.6 \end{bmatrix}$$ <p> where $$-0.5 < \tau < 0.5$$ <p> Plucking left leaves | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/fern_twiddle_2.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| $$f_4(P) = \begin{bmatrix} -0.15 &0.28 \\ 0.26 &0.24 \end{bmatrix}P + \begin{bmatrix} \tau \\ 0.44 \end{bmatrix}$$ <p> where $$-0.5 < \tau < 0.5$$ <p> Plucking right leaves | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/fern_twiddle_3.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

As an important note: the idea of modifying a resulting image by twiddling the knobs of an affine transform is the heart of many interesting methods, including fractal image compression where a low resolution version of an image is stored along with a reconstructing function set to generate high-quality images on-the-fly {{ "fractal-compression" | cite }}{{ "saupe1994review" | cite }}.
If this seems mystifying, don't worry!
We'll definitely come back to this soon, I just wanted to briefly mention it now so it's on everyone's mind as we move forward.


## Video Explanation

Here is a video describing the Barnsley fern:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/xoXe0AljUMA"
 frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; pic
ture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

Similar to the chapter on [iterated function systems](../IFS/IFS.md), the example code here will show a chaos game for the construction of an attractor;
however, in this case the attractor will be the Barnsley fern instead of the Sierpinski triangle.
The biggest differences between the two code implementations is that the Barnsley implementation must take into account the varying probabilities for choosing each function path and that we will be choosing an initial point that is *on* the attractor this time (namely $$(0,0)$$).

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/barnsley.jl)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/barnsley.rs)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/cpp/barnsley.cpp)
{% sample lang="c" %}
[import, lang:"c"](code/c/barnsley.c)
{% sample lang="java" %}
[import, lang:"java"](code/java/Barnsley.java)
{% sample lang="coco" %}
[import, lang:"coconut"](code/coconut/barnsley.coco)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/Barnsley.hs)
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

- The image "[IFS triangle 1](../IFS/res/IFS_triangle_1.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[IFS square 3](../IFS/res/IFS_square_3.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[Simple Barnsley fern](res/full_fern.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine random transform 0](res/affine_rnd_0.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine random transform 1](res/affine_rnd_1.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine random transform 2](res/affine_rnd_2.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine random transform 3](res/affine_rnd_3.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine fern transform 0](res/affine_fern_0.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine fern transform 1](res/affine_fern_1.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine fern transform 2](res/affine_fern_2.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Affine fern transform 3](res/affine_fern_3.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Fern twiddle 0](res/fern_twiddle_0.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Fern twiddle 1](res/fern_twiddle_1.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Fern twiddle 2](res/fern_twiddle_2.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Fern twiddle 3](res/fern_twiddle_3.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
