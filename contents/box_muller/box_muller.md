# The Box&mdash;Muller Transform

The Box&mdash;Muller transform holds a special pace in my heart as it was the first method I ever had to implement for my own research.
It was also marked my first time writing Fortran code, so it wasn't all sunshine and rainbows!

The purpose of this transformation is simple.
It takes a uniform (probably random) distribution and turns it into a Gaussian one.

ADD IMAGE

That's it.

It was originally developed by George Box (yes, Box is his last name) and Mervin Muller in 1958 and is one of the most common methods to create non-uniform, random distributions of points {{ "box1958" | cite }}.
It's particularly useful when initializing a distribution of particles for a physical, N-body simulation.
This chapter will be divided into a few subsections:

1. How to initialize the Box&mdash;Muller transform
2. How to use the Box&mdash;Muller transform in Cartesian coordinates
3. How to use the Box&mdash;Muller transform in Polar Coordinates

Note that for this chapter, we will be focusing on two dimensional transformations, as these are still relatively easy to understand and show how the method can be abstracted to higher dimensions.
There will be a brief discussion of how to do this in section 2.
Of course, there will be full code examples at the bottom.
So, let's get to it!

## How to initialize the Box&mdash;Muller transform

The main thing to mention here is that the Box&mdash;Muller transform requires some form of uniform distribution as it's input.
One obvious way to initialize a random distribution of points is to start with a grid, like so:

{% method %}
{% sample lang="jl" %}
[import:1-35, lang:"julia"](code/julia/box_muller.jl)
{% endmethod %}

This will create the following set of points for $$n=100$$:

<p>
    <img  class="center" src="res/grid.png" style="width:70%" />
</p>

To be honest, there are a bunch of ways to generate this exact same distribution.
Here, we simply walked backwards half of the grid size, determined the step size, and then placed a particle at each step.
Note that there is an inherent limitation with this method in that it only works for a square numbers.
For this, we just decided to round $$n$$ up to the nearest square to make a nice grid.
It's not the cleanest implementation, but the grid will mainly be used for debugging anyway, so it's ok to be a *little* messy here.

The real star of the show here is the uniform random distribution, which can be generated like this:

{% method %}
{% sample lang="jl" %}
[import:37-39, lang:"julia"](code/julia/box_muller.jl)
{% endmethod %}

This will create the following set of points for $$n=100$$:

<p>
    <img  class="center" src="res/rand_dist.png" style="width:70%" />
</p>

Ok, but how do we know this is uniform?
Good question!

The easiest way is to plot a histogram of a super large number of points.
If the random distribution is uniform, then all the bins should be roughly the same value.
The more points we have, the closer the bin values will be to each other.
Here is a set of images for $$n=100$$, $$1,000$$, and $$10,000$$ all in one dimension.


| $$100$$ | $$1,000$$ | $$10,000$$ |
|---------|-----------|------------|
|![100 points](res/rand100.png)|![1000 points](res/rand1000.png)|![10000 points](res/rand10000.png)|

It is clear that the 10,000 case looks the most uniform.
Once this test is complete, we can be fairly sure that the function we are using to generate a random distribution is uniform and ready for the next step of the process: actually using the Box&mdash;Muller transform!


## How to use the Box&mdash;Muller transform in Cartesian coordinates

The (2D) Cartesian version of the Box&mdash;Muller transform starts with 2 random input values ($$u_1$$ and $$u_2$$), both of which come from their own uniform distributions.
It then creates 2 output points ($$z_1$$ and $$z_2$$).
For this, $$u_1$$ is used to create a Gaussian distribution along some radial value $$r$$, and $$u_2$$ is used to spin that around a circle with some an angular component $$\theta$$, such that 

$$
\begin{align}
r &= \sqrt{-2\ln(u_1)} \\
\theta &= 2\pi u_2.
\end{align}
$$

Then $$z_1$$ and $$z_2$$ are essentially $$x,y$$ values of

$$
\begin{align}
z_1 &= r\cos(\theta) \\
z_2 &= r\sin(\theta).
\end{align}
$$

This can be more easily visualized in the following animation:

ADD ANIMATION

In general, this can be written qite simply in code like so:

ADD CODE

Which produces the following output

ADD IMAGE (WITH XY CROSS SECTIONS AS HISTOGRAMS)

Note that we have written this to work on a single set of input values, but this could also be written to read in the entire distribution of points all at once.
As this particular technique is usually implemented in parallel, it's up to you to decided what is the fastest for your own individual use-case.

For now, I would like to briefly touch on how to abstract this to different dimensions.

### What about 1D?

If you would like to use the Box&mdash;Muller transform in one dimension, things are relatively easy, just ignore $$u_2$$ and $$\theta$$, such that

$$
z = r =  \sqrt{-2\ln(u)}.
$$

There shouldn't be any funny business here.
We will leave the code modifications to the reader, but show the results:

ADD IMAGE

### Ok, what about for 3D?

For three dimensions, you would need to use spherical coordinates and one additional uniform distribution to create a third point $$u_3$$ and $$z_3$$.
For spherical coordinates, the additional angular value of $$\phi$$ only stretches to $$\pi$$, so the appropriate transforms would be

$$
\begin{align}
r &= \sqrt{-2\ln(u_1)} \\
\theta &= 2\pi u_2 \\
\phi &= \pi u_2.
\end{align}
$$

Then $$z_1$$, $$z_2$$, and $$z_3$$ are essentially $$x,y,z$$ values of

$$
\begin{align}
z_1 &= r\cos(\theta)sin(\phi) \\
z_2 &= r\sin(\theta)sin(\phi) \\
z_3 &= r\cos(\phi).
\end{align}
$$

Similar to the previous subsection, we will leave the code modifications to the reader and show the results:

ADD IMAGE

At this stage, we have a good idea of how the transform works, but some people shy away from the Cartesian method in-practice and instead opt for the polar form, which we will be talking about in the next section.

## How to use the Box&mdash;Muller transform in polar coordinates

Though the Cartesian form of the Box&mdash;Muller transform is relatively intuitive, the polar is essentially the same, but without the costly $$\sin$$ and $$\cos$$ operations.
In this case, we use the input values to create an initial value for $$r$$ (to be scaled later):

$$
r_0 = \sqrt{u_1^2 + u_2^2}.
$$

This means that we are essentially trying to transform our set of $$u$$ values into a new input value $$r_0$$ and this has a few important consequences:
1. To ensure that r_0 is also uniformly distributed, we must reject any values for $$u_1$$ and $$u_2$$ where $$r$$ is either $$0$$ or $$\gt 1$$

From here, we can use basic trigonometric identities to redefine the $$\sin$$ and $$\cos$$ to be

$$
\begin{align}
\cos(\theta) &= u_1/\sqrt{r_0} \\
\sin(\theta) &= u_2/\sqrt{r_0}.
\end{align}
$$

This changes the output equations to be

$$
\begin{align}
z_1 &= r\cos(\theta) = \sqrt{-2\ln(r_0)}\left(\frac{u_1}{\sqrt{r_0}}\right) = u_1 \sqrt{\frac{-2\ln(r_0)}{r_0}} \\
z_2 &= r\sin(\theta) = \sqrt{-2\ln(r_0)}\left(\frac{u_2}{\sqrt{r_0}}\right) = u_2 \sqrt{\frac{-2\ln(r_0)}{r_0}}.
\end{align}
$$

In code, this might look like this:

ADD CODE

Again, this is ultimately the same as the Cartesian method, except that it:
1. Rejects points outside the unit circle (rejection sampling)
2. Avoids costly $$\sin$$ and $$\cos$$ operations

Point 2 means that the polar method is way faster than the Cartesian one, but 1 is somewhat interesting in it's own right...

### Just how costly is rejection sampling anyway?

Essentially if you want to ensure there are $$n$$ particles in your final distribution of points, you probably want to wrap the polar Box&mdash;Muller transform in a `while` loop until $$n$$ points are found to be in the unit circle
In the [Monte Carlo chapter](../monte_carlo/monte_carlo.md) we calculated the value of $$\pi$$ by embedding it into a circle.
There, we found that the probability of a randomly chosen point falling within the unit circle to be $$\frac{\pi r^2}{(2r)^2} = \frac{pi}{4} \sim 78.54\%$$, shown in the image below:

ADD IMAGE

This means that this method rejects $$\sim 21.46\%$$ of points.
This also means that if you have a specific $$n$$ value you want for the final distribution, you will need $$\frac{1}{0.7853} \sim 1.273$$ more input values on average!

Ok, but is this fast enough to be worth it?
I also asked myself the same question and created the following benchmarks:

ADD TABLE OF BENCHMARKS for GPU / CPU

These were run with an Nvidia GTX 970 GPU and a Ryzen 3700X 16 core CPU.
The code can be found [here](ADD) and was written in Julia using the KernelAbstractions package for parallelization.
There are 2 variations of the polar code, one where each thread continually attempts to find a valid point, thereby filling a vector of $$n$$ elements exactly, and another that only partially fills the matrix, leaving all rejected entries as $$0$$.


## Video Explanation

Here is a video describing the Barnsley fern:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/xoXe0AljUMA"
 frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; pic
ture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

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
