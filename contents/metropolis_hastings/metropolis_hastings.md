# Metropolis Algorithm

The [Monte Carlo Integration](../monte_carlo_integration/monte_carlo_integration.html) method uses random numbers to approximate the area of pretty much any shape we choose. The Metropolis algorithm is a slightly more advanced Monte Carlo method which uses random numbers to approximate a probability distribution. What's really powerful about this approach is that you don't need to know the probability function itself - you just need a function which is _proportional_ to it. 

Why is this helpful? Well, we often have probability functions of the following form:

$$
P(\mathbf{x}) = \frac{f(\mathbf{x})}{\int_D f(\mathbf{x})d\mathbf{x}}
$$

where $$D$$ is the domain of $$P(\mathbf{x})$$, i.e., all possible values of the coordinates $$\mathbf{x}$$ for which $$P(\mathbf{x})$$ is defined. 

It's often easy for us to know $$f(x)$$. But the integral in the denominator can be quite difficult to calculate, even numerically.
This is especially true when the coordinates ($$\mathbf{x}$$) are multidimensional, and $$f(\mathbf{x})$$ is an expensive calculation. One example use case is a multidimensional physical system with energy $$E(\mathbf{x})$$, for which we know that $$f(x) = e^{-E(\mathbf{x})}$$, but $$P(x)$$ is almost always impossible to calculate. 

## A Random Walk in One Dimension

The Metropolis Algorithm is very similar to a random walk, so let's first see how we can get a distribution from a random walk.

<p>
	<img class="center" src="res/animated_random_walk.gif" alt="<FIG> Random Walk in 1D" style="width:80%"/>
</p>

The dot in the figure above is a "walker", whose initial position is $$x=0$$. The step size, $$g$$, is a random number in the interval $$(-1, 1)$$. To get the next position of the walker, we simply generate $$g$$ and add it to the current position. To get a distribution of $$x$$ from this walk, we can divide the domain into discrete locations or "bins" and count how often the walker visits each bin. Each time it visits a bin, the frequency for that bin goes up by one. Over many iterations, we get a frequency distribution of $$x$$. 

## A Random Walk With an Acceptance Criterion

The Metropolis algorithm works in a similar way to the Random Walk, but differs crucially in one way - after choosing a random step for the walker, a decision is made about whether to __accept__  or __reject__ the step based on the function $$f(x)$$. To understand how this works, let's call $$x_t$$ the position before the step, and $$x'$$ the position after it. We then define the probability of __accepting the step__ to be

$$
A = \min \left(\frac{f(x')}{f(x_t)}, 1\right)
$$

The $$\min$$ function above implies that $$A=1$$ if $$f(x') \gt f(x_t)$$, which means that the move will __always__ be accepted if it is toward a higher probability position. Otherwise, it will be accepted with a probability of $$f(x') / f(x_t)$$. If we create a histogram of this walk for some arbitrary target function $$P(x)$$, we can see from the figure below that the frequency starts to look very much like it after many iterations! 

<p>
	<img class="center" src="res/animated_metropolis.gif" alt="<FIG> Metropolis Walk in 1D" style="width:80%"/>
</p>

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
[import:4-13, lang:"python"](code/python/metropolis.py)
{% endmethod %}

Since this is an easy function to integrate, and hence get our target distribution $$P(x)$$ directly,  we can use it to verify the Metropolis algorithm. The plot of $$P(x)$$ in the figure below shows three different peaks of varying width and height, with some overlap as well.

<p>
	<img class="center" src="res/plot_of_P.png" alt="<FIG> Plot of P(x)" style="width:80%"/>
</p>

Next, we define our walker's symmetric step generating function. As in the Random Walker example, we will use a random number in the interval $$(-1,1)$$

{% method %}
{% sample lang="py" %}
[import:15-17, lang:"python"](code/python/metropolis.py)
{% endmethod %}

Finally, we choose the domain of $$x$$, and an initial point for $$ x_0 $$ ($$x_t$$ at $$t = 0$$) chosen randomly from the domain of $$x$$.

{% method %}
{% sample lang="py" %}
[import:34-35, lang:"python"](code/python/metropolis.py)
{% endmethod %}

### How to Iterate 

1. Generate new proposed position $$x' = x_t + g$$
2. Calculate the acceptance probability, 
$$
A = \min\left(1, \frac{f(x')}{f(x_t)}\right)
$$
3. Accept or reject:
	* Generate a random number $$u$$ between $$0$$ and $$1$$.
    * If $$ u \leq A $$, then __accept__ move, and set new position, $$x_{t+1} = x' $$
    * Otherwise, __reject__ move, and set new position to current, $$x_{t+1} = x_t $$
4. Increment $$t \rightarrow t + 1$$ and repeat from step 1.

The code for steps 1 to 3 is:

{% method %}
{% sample lang="py" %}
[import:19-31, lang:"python"](code/python/metropolis.py)
{% endmethod %}

The following plot shows the result of running the algorithm for different numbers of iterations ($$N$$), with the same initial position. The histograms are normalized so that they integrate to 1. We can see the convergence toward $$P(x)$$ as we increase $$N$$.

<p>
	<img class="center" src="res/multiple_histograms.png" alt="<FIG> multiple histograms" style="width:80%"/>
</p>


## Example Code
The following code puts everything together, and runs Metropolis algorithm for $$N$$ steps. All the positions visited by the algorithm are then written to a file, which can be later read and fed into a histogram or other density calculating scheme. 

{% method %}
{% sample lang="py" %}
[import, lang:"python"](code/python/metropolis.py)
{% endmethod %}


## Things to consider 

### Any symmetric function can be used to generate the step

So far the function $$g$$ we used for  generating the next step is a random number in the interval $$(-1,1)$$. However, this can be any function symmetric about $$0$$ for the above algorithm to work. For example, it can be a number randomly from a list of numbers like $$[ -3, -1, -1, +1, +1, +3]$$. In higher dimensions, the function should be symmetric in all directions, such as multidimensional Gaussian function. However, the choice of $$g$$ can affect how quickly the target distribution is achieved. The optimal choice for $$g$$ is not a trivial problem, and depends on the nature of the target distribution, and what you're interested in.


### A runaway walker

In the example above, the probability decays very quickly as $$\left|x\right| \rightarrow \infty$$. But sometimes, the function can flatten out and decay more slowly, so that the acceptance probability is always close to 1. This means it will behave a lot like a random walker in those regions, and may drift away and get lost! So it is a good idea to apply some boundaries beyond which $$f(x)$$ will simply drop to zero.



<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).


##### Images/Graphics
- The animation "[animated_random_walk](res/animated_random_walk.gif)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

- The animation "[animated_metropolis](res/animated_metropolis.gif)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

- The image "[plot_of_P](res/plot_of_P.png)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

- The image "[multiple_histograms](res/multiple_histograms.png)" was created by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

##### Text

The text of this chapter was written by [K. Shudipto Amin](https://github.com/shudipto-amin) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
