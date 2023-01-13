# Affine Transformations

Affine transformations are a class of mathematical operations that encompass rotation, scaling, translation, shearing, and several similar transformations that are regularly used for various applications in mathematics and computer graphics.
To start, we will draw a distinct (yet thin) line between affine and linear transformations before discussing the augmented matrix formalism typically used in practice.

## A quick intro to affine (and linear) transforms

Let us start with a provided point, $$(x,y)$$, on a two-dimensional plane.
If we treat this point as a $$1 \times 2$$ vector, we can transform it into another $$1 \times 2$$ vector by multiplying it with a $$2 \times 2$$ transformation matrix.
Similarly, a three-dimensional point could be seen as a $$1\times 3$$ vector and would need a $$3 \times 3 $$ transformation matrix.
These types of operations are known as linear transformations and are often notated as,

$$
\mathbf{v} = \mathbf{A}\mathbf{v}_0.
$$

Here, $$\mathbf{A}$$ is an $$n\times n$$ transformation matrix, where $$n$$ is the length of the input and output vectors, $$\mathbf{v_0}$$ and $$\mathbf{v}$$, respectively.
Though these transformations are powerful, all of them are centered about the origin.
Affine transformations extend linear transformations beyond this limitation and allow us to also translate our initial vector locations such that

$$
\textbf{v} = \mathbf{A}\mathbf{v}_0 + \ell.
$$

Here, $$\ell$$ is an $$n\times 1$$ translation vector.
To understand the power of these transformations, it is important to see them in practice:

| Description | Transform |
| ----------- | --------- |
| Scaling along $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a11_square_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Scaling along $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a22_square_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Shearing along $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a12_square_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Shearing along $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a21_square_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Translation along $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a13_square_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Translation along $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a23_square_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

For all of these visualizations, we show a set of 4 points that are assigned to the vertices of a square.
Initially, $$\mathbf{A}$$ is set to be the identity matrix and $$\ell = [0,0]$$, such that there is no transformation or translation to the input vectors.
From there, each element of $$\mathbf{A}$$ and $$\ell$$ are modified individually and the resulting transformation can be seen on the left.
The amount by which each element has been modified is shown numerically in the matrix representation and also as small dials underneath.

The hope is that these visualizations show that each element within $$\mathbf{A}$$ and $$\ell$$ are simply dials that can be manipulated to perform a specified transformation on the set of input vectors.
Of course, it is entirely possible to move more than one dial at a time, which is why it is worth diving into an example that everyone loves: rotation.

### Rotation: a special side-note

I will be honest, when I initially learned how to perform rotation with a linear transformation, I did not really understand how it worked.
For this reason, I think it is important to delve a bit deeper into this topic, hopefully providing an intuitive explanation for those who are new (and potentially those who already use the rotation matrix regularly, but do not fully understand it).

If someone were to take the set of dials shown above and mix them to create a rotational effect, they might start by shearing in one direction along $$x$$ and then another direction along $$y$$ which will create a "pseudo-rotation" effect.
This is definitely a step in the right direction, but if the shearing components are modified while the other components remain 1, the points will also move further away from the origin.
For this reason, an additional scaling along $$x$$ and $$y$$ is necessary.
This is shown in the following animation:

<div style="text-align:center">
<video style="width:100%" controls loop>
  <source src="res/semi_rotate_white.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

Here, we see that (at least for angles less than $$\pi/2$$), rotation is simply a matter of shearing in opposite directions and scaling accordingly.
Now the only question is, *"How do we know the amount we need to shear and scale?"*

Well, the answer is not particularly surprising.
If we want to rotate our points, we probably are already imagining this rotation along a circle with some angle $$\theta$$.
We know that the identity matrix should correspond to a non-rotated object with $$\theta = 0$$.
For this reason, we know that two elements should start at 1 (note: $$\cos(0) = 1$$) and the other two should start at 0 (note: $$\sin(0) = 0$$).
We also know that the shearing should happen in opposite directions, so we might guess that the rotation matrix would be:

$$
\mathbf{A}_{\text{rot}} = \begin{bmatrix}
\cos(\theta) & -\sin(\theta) \\
\sin(\theta) & \cos(\theta) \\
\end{bmatrix}
$$

In this case, the amount we want to shear should start at 0 when $$\theta = 0$$ and then go to $$\pm 1$$ when $$\theta = \pm \pi/2$$.
Meanwhile, the scale factor should start at 1 when $$\theta = 0$$ and go to $$0$$ when $$\theta = \pi/2$$.

This *seems* right, but it is worth dwelling on this a bit more.
If the scale factor is 0 at $$\pi/2$$, surely this means that all points on the square are also at 0, right?
After all, anything scaled by 0 should be 0!
Well, not exactly.
In this case,

$$
\mathbf{A} = \begin{bmatrix}
1 & 0 \\
0 & 1 \\
\end{bmatrix}
\rightarrow
\begin{bmatrix}
0 & -1 \\
1 & 0 \\
\end{bmatrix}
$$

This means that even though the scaling components are 0, the shear components are $$\pm 1$$.
This might still be a little confusing so let us multiply the vector $$[1,2]$$ with both of these matrices:

$$
\begin{align}
\begin{bmatrix}
1 & 0 \\
0 & 1 \\
\end{bmatrix}
\begin{bmatrix}
1 \\
2 \\
\end{bmatrix}
&=
\begin{bmatrix}
1 \\
2 \\
\end{bmatrix},\\


\begin{bmatrix}
0 & -1 \\
1 & 0 \\
\end{bmatrix}
\begin{bmatrix}
1 \\
2 \\
\end{bmatrix}
&=
\begin{bmatrix}
-2 \\ 
1 \\
\end{bmatrix}.

\end{align}
$$

Here, we see that when multiplying by the identity matrix, the vector remains the same, but when multiplying by the second matrix, the x and y components flip.
Essentially, all of the vector magnitude moved into the "shear" component, while none of it remains in the "scale" component.

My point is that even though it is useful to think of two of our dials as scale factors along $$x$$ and $$y$$, it does not necessarily paint the whole picture and it is important to consider how these different components work together.

Before continuing to show what the $$\mathbf{A}_{\text{rot}}$$ matrix does when applied to a square, it is worth considering two somewhat related matrices where the identity matrix is modified with only the $$\sin(\theta)$$ or $$\cos(\theta)$$ components.

| Description | Transform |
| ----------- | --------- |
| Just sines | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/sines_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Just cosines | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/cosines_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

Here, we see two completely different behaviors:

1. In the sine-only case, we see that as $$\theta$$ wraps around from $$0 \rightarrow 2\pi$$, the square seems to grow and rotate like expected, but at $$\pi/2$$, it somewhat abruptly decides to move in the other direction.
2. In cosine-only case, we see the square flip around entirely at $$\pi/2$$.

Before watching the next video, it is important to think for a little bit about how these two different interactions will work together in practice.
When you are ready, go ahead and click the play button:

<div style="text-align:center">
<video style="width:100%" controls loop>
  <source src="res/rotation_square_white.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

At least for me, it took some thinking to figure out why the two animations above create rotation when put together.
When thinking about it, it makes sense that at $$\pi/2$$, the sine component will start to encourage the square to slowly oscillate back towards the original position, but will be tugged in the opposite direction by the cosine component that has turned negative at the same time.
This "coincidence" is what creates a rotational effect.

Overall, the rotation matrix is a fun and interesting application to linear transformations that really helped me understand how the entire class of operations can be used to create more complicated movements.

### Guarantees of affine transformations

At this stage, we have discussed what affine transforms are from a functional perspective; however, (as always) there is a lot more to discuss.
This particular chapter is meant to provide an intuitive feel for the transformations for those who might need to use them for whatever application they need, so I am hesitant to dive too deeply into more rigorous definitions; however, it is important to talk about certain properties of affine transforms that make them suitable for a wide variety of applications.
Namely, affine transformations preserve the following:

1. **collinearity between points**. This means that any points that are on the same line before an affine transform must be on that same line after the transformation. The line can still change in slope or position.
2. **parallelism between lines**. Any lines parallel before the transform must also be parallel after.
3. **ratios of the lengths of parallel line segments**. This means if you have two different line segments, one of which is parameterized by $$p_1$$ and $$p_2$$, while the other is parameterized by $$p_3$$ and $$p_4$$, then $$\frac{\vec{p_1 p_2}}{\vec{p_3 p_4}}$$ must be the same before and after transformation.
4. **convexity of any transformed shape**. If a shape does not have any concave component (a point that points in towards its center), then it cannot have a concave component after the transformation.
5. **barycenters of the collection of points**. The barycenter is the collective center of mass of the system, like the balancing point for a plate. Essentially, there is an equal amount of "stuff" on either side of the barycenter. This location must remain at the same location relative to each point after transformation.

Again, there is a lot more we could talk about, but I feel we will leave more rigorous discussions for later if we need them for subsequent algorithms.
Instead, I believe it is useful to move on to a relatively common implementation of affine transformations: the augmented matrix formalism.

## Augmented matrix implementation

As stated before, affine transformations are basically a mix of a transformation matrix and translation.
For two-dimensional input vectors, the augmented matrix formalism combines both of these into a large $$3 \times 3$$ transformation matrix.
If you are like me, this might be a bit confusing.
After all, if the two-dimensional vector is described by a $$1 \times 2$$ array, then how do you do a matrix multiplication with a $$3 \times 3$$ array?

To be honest, the answer *feels* like a bit of a hack: we simply append a 1 to the end of the input, output, and translation vectors, such that:

$$
\begin{bmatrix}
\mathbf{v} \\
1 \\
\end{bmatrix}
=
\left[\begin{array}{@{}ccc|c@{}}
 & \mathbf{A} &  & \ell \\
0 & \cdots & 0 & 1 \\
\end{array}\right]
\begin{bmatrix}
\mathbf{v}_0 \\
1
\end{bmatrix}
$$

So, using 

$$
\begin{align}
\mathbf{v}_0 &= \begin{bmatrix}
1 \\ 
2 \\
\end{bmatrix} \\
\mathbf{A} &= \begin{bmatrix}
0 & -1 \\
1 & 0 \\
\end{bmatrix} \\
\ell &= \begin{bmatrix}
0 \\ 
0 \\
\end{bmatrix}
\end{align},
$$

we would perform the following computation:

$$
\begin{bmatrix}
\mathbf{v} \\
1 \\
\end{bmatrix}
=
\left[\begin{array}{@{}cc|c@{}}
0 & -1 & 0 \\
1 & 0 & 0 \\
0 & 0 & 1 \\
\end{array}\right]
\begin{bmatrix}
1 \\
2 \\
1
\end{bmatrix}
$$

Doing this, we find that $$\mathbf{v} = [-2,1]$$, just as we found in the previous example.
Ok, now we need to talk about why this works.

Appending the 1 to the end of the two-dimensional vectors essentially turn them into three-dimensional vectors, with the $$z$$ dimension simply set to be 1.
The easiest way to visualize this is by thinking of the top plane on a larger cube, so here are the same vector operations as before on that cube:

| Description | Transform |
| ----------- | --------- |
| Scaling along $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a11_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Scaling along $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a22_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Shearing along $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a12_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Shearing along $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a21_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Translation along $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a13_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Translation along $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a23_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

The shear and scaling operations seem about the same as before; however, the translation operations are now clearly a shear along the entire cube!
The only reason this acts as translation for two dimensions is because we only care about the slice through the cube at $$z=1$$.

Now, the reason I always feel this implementation is a bit hacky is because there is a little magic that everyone keeps quiet about: the last row in the matrix.
With all of the operations shown above, it was simply set to $$[0,0,1]$$ and never touched again...
But that is terribly unsatisfying!

What would happen if we actually moved those dials and modified the bottom row?
Well...

| Description | Transform |
| ----------- | --------- |
| Shearing along $$z$$ and $$x$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a31_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Shearing along $$z$$ and $$y$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a32_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |
| Scaling along $$z$$ | <div style="text-align:center"> <video style="width:100%" controls loop> <source src="res/a33_cube_white.mp4" type="video/mp4"> Your browser does not support the video tag. </video> </div> |

In this case, the first two components are shearing along $$z$$ and $$x$$ and $$z$$ and $$y$$, while the last component is a scale along $$z$$.
If someone was taking a picture from above, none of these transformations would be visible.
Because we are hyper-focused on the top-down view for affine transformations, none of these operations are technically affine; however, they are still linear, and it is still nice to show all possible linear transforms for the cube as well.

Finally, let us go back to the rotation example:

<div style="text-align:center">
<video style="width:100%" controls loop>
  <source src="res/rotation_cube_white.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

Here, we see that we can embed just about any affine transformation into three dimensional space and still see the same results as in the two dimensional case.
I think that is a nice note to end on: affine transformations are linear transformations in an $$n+1$$ dimensional space.

## Video Explanation

Here is a video describing affine transformations:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/E3Phj6J287o" frameborder="0" allow="accelerometer; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/main/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Images/Graphics
- The video "[A11 square](res/a11_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A22 square](res/a22_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A12 square](res/a12_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A21 square](res/a21_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A13 square](res/a13_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A23 square](res/a23_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Semi Rotate](res/semi_rotate_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Sines](res/sines_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Cosines](res/cosines_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Rotate Square](res/rotation_square_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A11 cube](res/a11_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A22 cube](res/a22_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A12 cube](res/a12_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A21 cube](res/a21_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A13 cube](res/a13_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A23 cube](res/a23_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A31 cube](res/a31_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A32 cube](res/a32_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[A33 cube](res/a33_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The video "[Rotation cube](res/rotation_cube_white.mp4)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

