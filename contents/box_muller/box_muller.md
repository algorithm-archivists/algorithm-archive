# The Box&mdash;Muller Transform

The Box&mdash;Muller transform holds a special pace in my heart as it was the first method I ever had to implement for my own research.

The purpose of this transformation is simple.
It takes a uniform (probably random) distribution and turns it into a Gaussian one.

<p>
    <img  class="center" src="res/cartesian_rand_transform.png" style="width:100%" />
</p>

That's it.

It was originally developed by George Box (yes, Box is his last name) and Mervin Muller in 1958 and is one of the most common methods to create a random, Gaussian distributions of points {{ "box1958" | cite }}.
It's particularly useful when initializing a distribution of particles for a physical, N-body simulation.
This chapter will be divided into a few subsections:

1. How to initialize the Box&mdash;Muller transform
2. How to use the Box&mdash;Muller transform in Cartesian coordinates
3. How to use the Box&mdash;Muller transform in Polar Coordinates

Note that for this chapter, we will be focusing on two dimensional transformations, as these are still relatively easy to understand and show how the method can be abstracted to higher dimensions.
There will also be a brief discussion of how to do this in section 2.
Of course, there will be full code examples at the bottom.
So, let's get to it!

## How to initialize the Box&mdash;Muller transform

The main thing to mention here is that the Box&mdash;Muller transform requires some form of uniform distribution as it's input.
One obvious way to initialize a random distribution of points is to start with a grid, like so:

{% method %}
{% sample lang="jl" %}
[import:3-40, lang:"julia"](code/julia/box_muller.jl)
{% endmethod %}

This will create the following set of points for $$n=100$$:

<p>
    <img  class="center" src="res/grid.png" style="width:70%" />
</p>

To be honest, there are a bunch of ways to generate this exact same distribution.
Here, we simply walked backwards half of the grid size, determined the step size, and then placed a particle at each step.
Note that there is an inherent limitation with this method in that it only works for a square numbers.
Because of this, we decided to round $$n$$ up to the nearest square to make a nice grid.
It's not the cleanest implementation, but the grid will mainly be used for debugging anyway, so it's OK to be a *little* messy here.

The real star of the show here is the uniform random distribution, which can be generated like this:

{% method %}
{% sample lang="jl" %}
[import:42-45, lang:"julia"](code/julia/box_muller.jl)
{% endmethod %}

This will create the following set of points for $$n=100$$:

<p>
    <img  class="center" src="res/rand_dist.png" style="width:70%" />
</p>

OK, but how do we know this is uniform?
Good question!

The easiest way is to plot a histogram of a super large number of points.
If the random distribution is uniform, then all the bins should be roughly the same value.
The more points we have, the smaller the percent difference between the bins will be.
Here is a set of images for $$n=100$$, $$1,000$$, and $$10,000$$ all in one dimension:


| $$100$$ | $$1,000$$ | $$10,000$$ |
|---------|-----------|------------|
|![100 points](res/rand100.png)|![1000 points](res/rand1000.png)|![10000 points](res/rand10000.png)|

It is clear that the 10,000 case looks the most uniform.
Note that for two dimensions, the same logic applies, but we need to create separate histograms for the $$x$$ and $$y$$ coordinates.

Once this test is complete, we can be fairly sure that the function we are using to generate a random distribution is uniform and ready for the next step of the process: actually using the Box&mdash;Muller transform!


## How to use the Box&mdash;Muller transform in Cartesian coordinates

The (two dimensional) Cartesian version of the Box&mdash;Muller transform starts with two random input values ($$u_1$$ and $$u_2$$), both of which come from their own uniform distributions that are between $$0$$ and $$1$$.
It then creates two output points ($$z_1$$ and $$z_2$$).
For this, $$u_1$$ is used to create a Gaussian distribution along some radial value $$r$$, and $$u_2$$ is used to spin that around a circle with some angular component $$\theta$$, such that 

$$
\begin{align}
r &= \sqrt{-2\ln(u_1)} \\
\theta &= 2\pi u_2.
\end{align}
$$

Looking at these equations, $$\theta$$ seems to make a decent amount of sense.
After all, angles typically vary from $$0 \rightarrow 2\pi$$, and our input distribution varies from $$0 \rightarrow 1$$, so we can get some value between $$0$$ and $$2\pi$$ by multiplying $$2\pi$$ by one of our input values.

So what about $$r$$?
Well, remember that if we want $$u$$ to be in a Gaussian form, then we might say something like, $$u = e^{-\frac{r^2}{2}}$$, so if we solve this for $$r$$, we get $$r=\sqrt{-2\ln(u)}$$.

From these values, we can calculate our new $$x,y$$ points as,

$$
\begin{align}
x &= r\cos(\theta) \\
y &= r\sin(\theta).
\end{align}
$$

This can be more easily visualized in the following animation:

ADD ANIMATION

Finally, in order to specify the size and shape of the generated Gaussian distribution, we can use the standard deviation, $$\sigma$$, and the mean, $$\mu$$, like so:

$$
\begin{align}
z_1 &= x\sigma + \mu \\
z_2 &= y\sigma + \mu.
\end{align}
$$

In general, this can all be written in code like so:

{% method %}
{% sample lang="jl" %}
[import:48-56, lang:"julia"](code/julia/box_muller.jl)
{% endmethod %}

Which produces the following output

<p>
    <img  class="center" src="res/cartesian_rand_transform.png" style="width:100%" />
    <img  class="center" src="res/cartesian_grid_transform.png" style="width:100%" />
</p>

Note that we have written the code to work on a single set of input values, but it could also be written to read in the entire distribution of points all at once.
As this particular technique is usually implemented in parallel, it's up to you to decided which is the fastest for your own individual use-case.

For now, I would like to briefly touch on how to abstract this to different dimensions.

### What about 1D?

If we would like to use the Box&mdash;Muller transform in one dimension, things are relatively easy, just ignore $$u_2$$ and $$\theta$$, such that

$$
z = \sigma r + \mu = \sigma\sqrt{-2\ln(u)} + \mu.
$$

There shouldn't be any funny business here.
We will leave the code modifications to the reader, but show the results:

ADD IMAGE

### OK, what about for 3D?

For three dimensions, we need to use spherical coordinates and one additional uniform distribution ($$u_3$$) to create a third point $$z_3$$.
Also, the additional angular value of $$\phi$$ only stretches to $$\pi$$, so the appropriate transforms would be

$$
\begin{align}
r &= \sqrt{-2\ln(u_1)} \\
\theta &= 2\pi u_2 \\
\phi &= \pi u_3.
\end{align}
$$

Then $$x,y,z$$ are

$$
\begin{align}
x &= r\cos(\theta)\sin(\phi) \\
y &= r\sin(\theta)\sin(\phi) \\
z &= r\cos(\phi),
\end{align}
$$

and 

$$
\begin{align}
z_1 &= \sigma x + \mu \\ 
z_2 &= \sigma y + \mu \\ 
z_3 &= \sigma z + \mu.
\end{align}
$$

Similar to the previous subsection, we will leave the code modifications to the reader and show the results:

ADD IMAGE

At this stage, we have a good idea of how the transform works, but some people shy away from the Cartesian method in practice and instead opt for the polar form, which will discussed next!

## How to use the Box&mdash;Muller transform in polar coordinates

The Cartesian form of the Box&mdash;Muller transform is relatively intuitive
The polar method is essentially the same, but without the costly $$\sin$$ and $$\cos$$ operations.
In this case, we use the input values to create an initial radial point (to be scaled later):

$$
r_0 = \sqrt{u_1^2 + u_2^2}.
$$

This means that we are essentially trying to transform our set of $$u$$ values into a new input value $$r_0$$.
To do this, we need to start with a uniformly distributed *circle*, so we must reject any values for $$u_1$$ and $$u_2$$ where $$r$$ is either $$0$$ or $$\gt 1$$.
This also means that the initial distributions of $$u_1$$ and $$u_2$$ must range from $$-1 \rightarrow +1$$.

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
x &= r\cos(\theta) = \sqrt{-2\ln(r_0)}\left(\frac{u_1}{\sqrt{r_0}}\right) = u_1 \sqrt{\frac{-2\ln(r_0)}{r_0}} \\
y &= r\sin(\theta) = \sqrt{-2\ln(r_0)}\left(\frac{u_2}{\sqrt{r_0}}\right) = u_2 \sqrt{\frac{-2\ln(r_0)}{r_0}}.
\end{align}
$$

Again, the final values are:

$$
\begin{align}
z_1 &= \sigma x + \mu \\
z_2 &= \sigma y + \mu.
\end{align}
$$

This can be more easily visualized in the following animation:

ADD ANIMATION

In code, this might look like this:

{% method %}
{% sample lang="jl" %}
[import:60-71, lang:"julia"](code/julia/box_muller.jl)
{% endmethod %}

This will produce the following output:

<p>
    <img  class="center" src="res/polar_rand_transform.png" style="width:100%" />
    <img  class="center" src="res/polar_grid_transform.png" style="width:100%" />
</p>

Again, this is ultimately the same as the Cartesian method, except that it:
1. Rejects points outside the unit circle (also called rejection sampling)
2. Avoids costly $$\sin$$ and $$\cos$$ operations

Point 2 means that the polar method *should be* way faster than the Cartesian one, but rejection sampling is somewhat interesting in it's own right...

### Just how costly is rejection sampling anyway?

Let's imagine we want to have a final Gaussian with $$n$$ particles in it.
With the Cartesian Box&mdash;Muller method, this is easy: start the initial distribution(s) with $$n$$ particles and then do the transform.
Things *can* be just as easy with the Polar Box&mdash;Muller method as well, so long as we start with a uniformly distributed circle instead of a uniformly distributed square.
That is to say, so long as we do the rejection sampling before-hand, the Polar Box&mdash;Muller method will always be more efficient.
To be fair, there are methods to generate a uniform distribution of points within a circle without rejection sampling, but let's assume that we require rejection sampling for this example

This means that someone somehow needs to do the rejection sampling for the Polar method, which is sometimes a painful process.
This also means that the Box&mdash;Muller method can be used to teach some of the fundamentals of General-Purpose GPU computing.
Note that because of the specificity of this problem, all the code in this subsection will be in Julia and using the package KernelAbstractions.jl, which allows us to execute the same kernels on either CPU or GPU hardware depending on how we configure things.

Let's first consider the case where we do the rejection sampling as a part of the polar Box&mdash;Muller kernel instead of as a pre-processing step.
In this case, we can imagine 2 separate ways of writing our kernel:
1. With replacement: In this case, we *absolutely require* the final number of points in our Gaussian distribution to be $$n$$, so if we find a point outside of the unit circle while running the kernel, we will "re-roll" again for a new point that *is* within the circle.
2. Without replacement: This means that we will start with a uniform distribution of $$n$$ points, but end with a Gaussian of $$m < n$$ points. In this case, if we find a point outside of the unit circle while running the kernel, we just ignore it by setting the output values to NaNs (or something).

OK, so first with replacement:

[import:70-84, lang:"julia"](code/julia/performance.jl)

This is an awful idea for a number of reasons.
Here are a few:
1. If we find a point outside of the unit circle, we have to continually look for new points until we *do* find one inside of the circle. This means that some threads might take literally forever to find a new point (if we are really unlucky).
2. To generate new points, we need to re-generate a uniform distribution, but what if our uniform distribution is not random? What if it's a grid (or something similar) instead? In this case, we really shouldn't look for a new point on the inside of the circle as all those points have already been accounted for.
3. The `rand()` function is kinda tricky on some parallel platforms (like GPUs) and might not work out of the box. In fact, the implementation shown above can only be run on the CPU.

OK, fine.
I don't think anyone expected a kernel with a `while` loop inside of it to be fast.
So what about a method without replacement?
Surely there is no problem if we just ignore the `while` loop altogether!
Well, the problem with this approach is a bit less straightforward, but first, code:

[import:53-68, lang:"julia"](code/julia/performance.jl)

To start discussing why the kernel without replacement is *also* a bad idea, let's go back to the [Monte Carlo chapter](../monte_carlo/monte_carlo.md), where we calculated the value of $$\pi$$ by embedding it into a circle.
There, we found that the probability of a randomly chosen point falling within the unit circle to be $$\frac{\pi r^2}{(2r)^2} = \frac{pi}{4} \sim 78.54\%$$, shown in the visual below:

<p>
    <img  class="center" src="../monte_carlo_integration/res/monte_carlo.gif" style="width:60%"/>
</p>

This means that a uniform distribution of points within a circle will reject $$\sim 21.46\%$$ of points on the square.
This also means that if we have a specific $$n$$ value we want for the final distribution, we will need $$\frac{1}{0.7853} \sim 1.273 \times$$ more input values on average!

No problem!
In this hypothetical case, we don't need *exactly* $$n$$ points, so we can just start the initial distributions with $$1.273 \times n$$ points, right?

Right.
That will work well on parallel CPU hardware, but on the GPU this will still have an issue.

On the GPU, computation is all done in parallel, but there is a minimum unit of parallelism called a *warp*.
The warp is the smallest number of threads that can execute something in parallel and is usually about 32.
This means that if an operation is queued, all 32 threads will do it at the same time.
If 16 threads need to execute something and the other 16 threads need to execute something else, this will lead to *warp divergence* where 2 actions need to be performed instead of 1:

<p>
    <img  class="center" src="res/warp_divergence.png" style="width:100%" />
</p>

In this image, every odd thread needs to perform the pink action, while the even threads need to perform the blue action.
This means that 2 separate parallel tasks will be performed, one for the even threads, another for the odd threads.
This means that if $$\ell$$ operations are queued, it could take $$\ell\times$$ as long for all the threads to do their work!
This is why `if` statements in a kernel can be dangerous!
If used improperly, they can cause certain threads in a warp to do different things!

A good question here is, "why can't we modify the warp so that it only uses odd threads or even threads?"
This would prevent the warp divergence entirely, but it comes at the cost of more complicated memory operations, which is an entirely different problem.
For the most part, even it our warp does diverge, it is a good idea to make sure that each thread is working on it's own array element without complex accessing patterns.

If we look at the above kernel, we are essentially asking $$78.53\%$$ of our threads to do something different than everyone else, and because we are usually inputting a uniform random distribution, this means that *most* warps will have to queue up 2 parallel actions instead of 1.
Now we need to pick our poison: slow $$\sin$$ and $$\cos$$ operations or warp divergence.

The only way to know which is better is to perform benchmarks, which we will show in a bit, but there is one final scenario we should consider: what about doing the rejection sampling as a pre-processing step?
This would mean that we pre-initialize the polar kernel with a uniform distribution of points in the unit circle.
This means no warp divergence, so we can get the best of both worlds, right?

Well, not exactly.
The polar Box&mdash;Muller method will definitely be faster, but again: someone somewhere needed to do rejection sampling and if we include that step into the process, things become complicated again.
The truth is that this pre-processing step is difficult to get right, so it might require a chapter in it's own right.

In many cases, it's worth spending a little time before-hand to make sure subsequent operations are fast, but in this case, we only have a single operation, not a set of operations.
The Box&mdash;Muller method will usually only be used once at the start of the simulation, which means that the pre-processing step of rejection sampling might end up being overkill.

No matter the case, benchmarks will show the true nature of what we are dealing with here:

| Method                    | CPU                    | GPU                    |
| ------------------------- | ---------------------- | ---------------------- |
| Cartesian                 | $$385.819 \pm 1.9$$ms  | $$19.347 \pm 0.618$$ms |
| Polar without replacement | $$273.308 \pm 2.81$$ms | $$26.712 \pm 0.592$$ms |
| Polar with replacement    | $$433.644 \pm 2.64$$ms | NA                     |

These were run with an Nvidia GTX 970 GPU and a Ryzen 3700X 16 core CPU.
Again, the code can be found [here](code/julia/performance.jl) and was written in Julia using the KernelAbstractions.jl package for parallelization.
For these benchmarks, we used Julia's inbuild benchmarking suite from BenchmarkTools, making sure to sync the GPU kernels with `CUDA.@sync`.
We also ran with $$4096^2$$ input points.

Here, we see an interesting divergence in the results.
On the CPU, the polar method is *always* faster, but on the GPU, both methods are comparable.
I believe this is the most important lesson to be learned from the Box&mdash;Muller method: sometimes, no matter how hard you try to optimize your code, different hardware can provide radically different results!
It's incredibly important to benchmark code to make sure it is actually as performant as you think it is!

## Example Code

The example code here is straightforward: we start with a uniform distribution of points (both on a grid and a uniform random distribution) and then we preform the Box&mdash;Muller transform to see how far off it is from the Gaussian we expect.

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/box_muller.jl)
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
