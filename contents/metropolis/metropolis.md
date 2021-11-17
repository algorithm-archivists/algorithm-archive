# Metropolis Algorithm

The [Monte Carlo Integration](../monte_carlo_integration/monte_carlo_integration.html) method uses random numbers to approximate the area of pretty much any shape we choose. The Metropolis algorithm {{ "metropolis1953equation" | cite }} is a slightly more advanced Monte Carlo method which uses random numbers to approximate a [probability distribution](../probability/distributions/distributions.md).

Here we will use the Metropolis algorithm to approximate a __probability density function__, as described in [this chapter](../probability/distributions/distributions.md), which is of the form,


$$
P(\mathbf{x}) = \frac{f(\mathbf{x})}{\displaystyle\int_D f(\mathbf{x})d\mathbf{x}}
$$

where $$D$$ is the domain of $$P(\mathbf{x})$$, i.e., all possible values of the $$\mathbf{x}$$ for which $$P(\mathbf{x})$$ is defined; and $$f(\mathbf{x})$$ is some a function that is proportional to $$P(x)$$, such as a frequency distribution of $$\mathbf{x}$$. A one-dimensional example is the __normal distribution__, or __Gaussian distribution__, given by

$$
P(x) = \frac{e^{-x^2}}{\displaystyle\int_{-\infty}^{\infty} e^{-x^2} dx} = \frac{1}{\sqrt{\pi}} e^{-x^2}
$$


In practice, it's often easy for us to know $$f(x)$$, but the integral in the denominator can be quite difficult to calculate, even numerically. This is especially true when the coordinates ($$\mathbf{x}$$) are multidimensional, and $$f(\mathbf{x})$$ is an expensive calculation, as we shall see in the examples below.

## An  example application

 One example of a complicated probability function arises when considering a physical system of $$N$$ number of particles. These could be atoms, molecules, or even star systems! For such systems, we can usually describe the __potential energy__ {{ "potential_energy_wiki" | cite }} of the system as a function of the coordinates of all particles, $$\mathbf{x}$$,

$$
E(\mathbf{x}) = E(x_1, y_1, z_1, x_2, y_2, z_2, ... ,x_N, y_N, z_N) 
$$

where $$x_i, y_i, z_i$$ are the spatial coordinates of particle $$i$$. So altogether there are $$3N$$ coordinates - already this is complicated, but it doesn't end there!

The physicist Ludwig Boltzmann {{ "ludwig_boltzmann_wiki" | cite }} discovered that when such a system is in equilibrium at some temperature $$T$$, you can describe the probability density of the system for any set of coordinates $$\mathbf{x}$$ using, {{ "boltzmann_distribution_wiki" | cite }} 

$$
P(\mathbf{x}) = \frac{\displaystyle \exp\left[{\displaystyle\frac{-E(\mathbf{x})}{T} } \right]} {Q}
$$

where $$Q$$ is the [normalization constant](../probability/distributions/distributions.md),
$$
Q = \int_D \exp\left[{\displaystyle\frac{-E(\mathbf{x})}{T} } \right] d\mathbf{x}
$$

We can already see that the probability density function is quite complicated, particularly because of $$Q$$. Almost always, no analytical solution exists to the integral in $$Q$$, and the numerical integration is unfeasible. 

To see that $$Q$$ is unfeasible to calculate, imagine there are just 10 particles which all exist in a 1D world, restricted to a line segment.

<p>
	<img class="center" src="res/1D_particles.png" style="width:100%" alt="<FIG> 1D particles">
</p>

This means that the energy $$E(\mathbf{x})$$ of the system is a 10D function, as there are 10 coordinates in total. Now imagine that we divide the 1D line segment into only 50 different intervals, allowing each particle to take on 50 different positions. This is equivalent to dividing the length of a football field into intervals of about 2 meters - not a resolution you'd wanna watch a game in! Even with such poor resolution, the number of different combinations of positions is $$10^{50}$$ - a colossal number indeed! Even if a single computation of $$E(\mathbf{x})$$ took only 1 nanosecond on a single processor, _even_ with all the processors in the world running in parallel, calculating $$Q$$ would still take longer than the age of the universe - by many orders of magnitude!

What's really powerful about the Metropolis approach is that you don't need to know the probability function itself - you just need a function which is _proportional_ to it. What this means for the Botlzmann distribution is that you only need to know the term,

$$
f(\mathbf{x}) = \exp\left[{\displaystyle\frac{-E(\mathbf{x})}{T} } \right]
$$

And the Metropolis algorithm can bypass calculation of $$Q$$ altogether and use $$f(x)$$ to generate a distribution of $$x$$ which follows the probability density $$P(x)$$. In other words, it can sample values of $$x$$ in such away that the probability of sampling $$x$$ will follow the actual distribution $$P(x)$$ - the more likely a value of $$x$$ is according to $$P(x)$$, the more likely it will be sampled by the Metropolis algorithm. This fact dramatically reduces the number of samples needed to approximate the probability distribution.

Of course to be truly proportional to $$P(x)$$ the Metropolis algorithm would still take some time before it converges - but it scales very well with the dimensionality of $$\mathbf{x}$$, which is important since the number of particles can get large in physical systems. Furthermore, we often don't care about the _entire_ space of possibilities. We usually care about a local region of $$P(x)$$, where the probability is relatively high, and so you would have "realistic" positions for the particles (it turns out reality tries to exist in a high probability state, who would've guessed?). Because the Metropolis algorithm tends toward high probability regions, it's good at sampling those regions and providing us with a realistic collection of systems.

Finally, the Metropolis algorithm can be modified or implemented in other algorithms, and forms the basis of many advanced sampling algorithms. The most popular is probably the Metropolis-Hastings algorithm {{ "hastings1970monte" | cite }} which is fundamentally the same. Other algorithms that implement the algorithm are Metropolis-adjusted Langevin algorithm {{ "mala_wiki" | cite }}, and Hamiltonian Monte Carlo {{ "hmc_wiki" | cite }}, to name a few. They are often used for physical systems that follow a Boltzmann distribution.


## A Random Walk in One Dimension

In the rest of this chapter, we will look at 1D examples to understand the Metropolis algorithm. Although the algorithm is not particularly efficient in just one dimension, it is much easier to understand and learn how to implement than in higher dimensions. The Metropolis algorithm is very similar to a random walk, so let's first see how we can get a distribution from a random walk.

<div style="text-align:center">
<video style="width:80%" controls loop>
  <source src="res/animated_random_walk.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

The dot in the figure above is a "walker", whose initial position is $$x=0$$. The step size, $$g$$, is a random number in the interval $$(-1, 1)$$. To get the next position of the walker, we simply generate $$g$$ and add it to the current position. To get a distribution of $$x$$ from this walk, we can divide the domain into discrete locations or "bins" and count how often the walker visits each bin. Each time it visits a bin, the frequency for that bin goes up by one. Over many iterations, we get a frequency distribution of $$x$$. 

## A Random Walk With an Acceptance Criterion

The Metropolis algorithm works in a similar way to the random walk, but differs crucially in one way - after choosing a random step for the walker, a decision is made about whether to __accept__  or __reject__ the step based on the function $$f(x)$$. To understand how this works, let's call $$x_t$$ the position before the step, and $$x'$$ the position after it. We then define the probability of __accepting the step__ to be

$$
A = \min \left(\frac{f(x')}{f(x_t)}, 1\right)
$$

The $$\min$$ function above implies that $$A=1$$ if $$f(x') \gt f(x_t)$$, which means that the move will __always__ be accepted if it is toward a higher probability position. Otherwise, it will be accepted with a probability of $$f(x') / f(x_t)$$. If we create a histogram of this walk for some arbitrary target function $$P(x)$$, we can see from the figure below that the frequency starts to look very much like it after many iterations! 

<div style="text-align:center">
<video style="width:80%" controls loop>
  <source src="res/animated_metropolis.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

Although convergence occurs eventually, not all parts of the distribution achieve convergence quickly. Note from the animation above, that the walker very quickly replicates the distribution of the two peaks on the left, but takes quite a while to even reach the third peak to the right. This is because there is a long "barrier" of low probability region between the third peak and second peak. This may not necessarily be a bad thing - sometimes one might want to estimate how long something takes to transition from one state to another, and often these peaks represent such 'states'. So averaging over many metropolis runs may give some estimate of these transition times. And if global sampling is the goal, the process of exploration could be sped up by choosing larger step sizes for the walker, for example by choosing step size $$g$$ from an interval like $$(-3,3)$$ instead of $$(-1,1)$$. 


## The Algorithm for a One Dimensional Example

### The Initial Setup

Let our target distribution be
$$
P(x) = \frac{f(x)}{\int_{-10}^{10} f(x)}
$$

where $$f(x)$$ is the function we know and is given by
$$
f(x) = 10e^{-4(x+4)^2} + 3e^{-0.2(x+1)^2} + e^{-2(x-5)^2}
$$

The code for defining this function is given below.

{% method %}
{% sample lang="py" %}
[import:4-18, lang:"python"](code/python/metropolis.py)
{% endmethod %}

Since this is an easy function to integrate, and hence get our target distribution $$P(x)$$ directly,  we can use it to verify the Metropolis algorithm. The plot of $$P(x)$$ in the figure below shows three different peaks of varying width and height, with some overlap as well.

<p>
	<img class="center" src="res/plot_of_P.png" alt="<FIG> Plot of P(x)" style="width:80%"/>
</p>

Next, we define our walker's symmetric step generating function. As in the random walk example, we will use a random real number between $$-1$$ and $$+1$$ as the step size. 

{% method %}
{% sample lang="py" %}
[import:20-22, lang:"python"](code/python/metropolis.py)
{% endmethod %}

However, $$g$$ can be any function symmetric about $$0$$ for the above algorithm to work. For example, it can be a number chosen randomly from a discrete list, such as $$[ -3, -1, -1, +1, +1, +3]$$. It can also be a number chosen from a symmetric continuos distribution, like the Gaussian, $$e^{-x^2}$$. In higher dimensions, the function should be spherically symmetric, such as multidimensional Gaussian function, $$e^{-(x^2 +y^2 + ...)}$$. Whatever function you choose, there are at least a couple of things to note:
1. If the function $$g$$ is discrete, you will only sample discrete values. For example, if $$g$$ returns only -1 or +1, and nothing in between, you will sample only integer steps away from the initial $$x_0$$. 
2. The average step size really matters! A small step-size means the walker will carefully sample nearby regions more, but will walk more slowly, so might not be good at exploring far and wide. On the other hand, a walker with a large step size may not sample nearby regions accurately - and actually has a higher chance of being rejected if the walker is already in a high probability region, since the acceptance ratio is more drastic for large steps. The effect of step-size on the walker's efficiency is far from obvious! 

How to choose $$g$$ is in itself a research field and depends on what the goal of the sampling is. Some methods even use an "adaptive" method where $$g$$ is "trained" on-the-fly using a learning algorithm! Some of these methods and others are discussed in Ref. {{ "rosenthal2011optimal" | cite }} and Ref. {{ "gareth2001optimal" | cite }}. In a lot of cases, people just use trial and error, as the algorithm is not too difficult to implement.

After chosing $$g$$, we are almost ready to iterate. We just need to choose the domain of $$x$$, and an initial point for $$ x_0 $$ ($$x_t$$ at $$t = 0$$) chosen randomly from the domain of $$x$$.

{% method %}
{% sample lang="py" %}
[import:70-71, lang:"python"](code/python/metropolis.py)
{% endmethod %}

### How to Iterate 

1. Generate new proposed position $$x' = x_t + g$$
2. Calculate the acceptance probability, 
$$
A = \min\left(1, \frac{f(x')}{f(x_t)}\right)
$$
3. Accept proposal, $$x'$$ with probability $$A$$. If your programming language doesn't have a built-in method for this,
	* Generate a random number $$u$$ between $$0$$ and $$1$$.
    * If $$ u \leq A $$, then __accept__ move, and set new position, $$x_{t+1} = x' $$
    * Otherwise, __reject__ move, and set new position to current, $$x_{t+1} = x_t $$
4. Increment $$t \rightarrow t + 1$$ and repeat from step 1.

The code for steps 1 to 3 is:

{% method %}
{% sample lang="py" %}
[import:34-42, lang:"python"](code/python/metropolis.py)
{% endmethod %}

The following plot shows the result of running the algorithm for different numbers of iterations ($$N$$), with the same initial position. The histograms are normalized so that they integrate to 1. We can see the convergence toward $$P(x)$$ as we increase $$N$$.

<p>
	<img class="center" src="res/multiple_histograms.png" alt="<FIG> multiple histograms" style="width:80%"/>
</p>


## Example Code
The following code puts everything together, and runs Metropolis algorithm for a number of  steps given by `num_steps`. All the positions visited by the algorithm are then written to a file, which can be later read and fed into a histogram or other density calculating scheme. The code also incorporates a few tests of the algorithm using the `test_metropolis_iterate` method. This test will create a normalized density histogram from the generated data, and compare it to $$P(x)$$ using the Root Mean Square Deviations metric {{ "rmsd_wiki" | cite }}.

{% method %}
{% sample lang="py" %}
[import, lang:"python"](code/python/metropolis.py)
{% endmethod %}



<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

### Bibliography

{% references %} {% endreferences %}

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).


##### Images/Graphics
- The animation "[Animated Random Walk](res/animated_random_walk.gif)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

- The animation "[Animated Metropolis](res/animated_metropolis.gif)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

- The image "[Plot of P(x)](res/plot_of_P.png)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

- The image "[Multiple Histograms](res/multiple_histograms.png)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

##### Text

The text of this chapter was written by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
