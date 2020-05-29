# Domain coloring

Domain coloring is a much more complicated plotting technique than those outlined in the [plotting chapter](../plotting/plotting.md) and is used to plot complex functions where both the input and output have imaginary and real components.
As stated in similar chapters, when discussing plotting methods, we will focus on plotting languages, instead of languages meant for number crunching or other purposes.
That is to say that this chapter will certainly have a code implementation in gnuplot, but it will not likely have an implementation in C, Fortran, or Java because these chapters do not have plotting capabilities in-built.

Imagine the following function: $$f(z) = z^2$$.
In this case, we could create a plot that looks like this:

<p>
    <img  class="center" src="res/z2.png" style="width:70%" />
</p>

This indicates that for various input values along $$z$$, we have different function outputs from $$f(z)$$.

Now let's imagine another function with complex input $$(z \in \mathbb{C})$$, but a purely real output $$(f(z) \in \mathbb{R})$$:

$$f(z) = |z|$$

In this case, each complex input has a real output.
This can be plotted as a two-dimensional dataset like so:

<p>
    <img  class="center" src="res/absz.png" style="width:84%" />
</p>

Here, the $$x$$-axis and $$y$$-axis represent the real and imaginary components of the input variable, respectively.

At this point, we can start to see the problem.
If the output of $$f(z)$$ also requires plotting of the real and imaginary components, then we would need four dimensions to appropriately represent complex functions, one axis for their real component and another for their imaginary component of both the input ($$z$$) and the output of $$f(z)$$!
Unfortunately, feeble human minds are incapable of understanding four spatial dimensions without projecting onto lower dimensionality, so we need to improvise.

We do this by assuming the complex output can be represented in the following form:

$$z = re^{i \theta} = r(\cos(\theta) + i\sin(\theta)$$

where, $$r$$ is a complex magnitude and $$\theta$$ is a complex phase.
This is the formula for a circle in the complex plane and we can easily find $$r$$ and $$\theta$$ like so:

$$
\begin{align}
    r &= \sqrt{\text{Re}(z)^2 + \text{Im}(z)^2} \\ 
    \theta &= \text{atan}(\frac{\text{Im}(z)}{\text{Re}(z)})
\end{align}
$$

Once we have our complex function output in this form, we then color the output domain according to a number space with 2 or more dimensions, for example black and white, RGB (Red, Green, Blue), or HSV (Hue, Saturation, Value).
The choice of color space is completely dependent on what the users feel is most visually intuitive.
In any case, one dimension of the color system will be used to represent the complex magnitude of the output and another dimension of the color system will be used to represent the complex phase.
The $xy$ grid will be representing the real and imaginary inputs to these functions.
That is to say, we plug every value in the 2D complex plane into the function and then color each pixel based on the function output.

As an example, let's again look at the fuction $$f(z) = z^2$$, but in this case $$z \in \mathbb{C}$$.


THESE WORK LIKE SO

So if we are using HSV, then we set the color values for each pixel in our image like so:

$$
\begin{align}
    H &= \\
    S &= \\
    V &= 100\%
\end{align}
$$

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Images/Graphics

##### Pull Requests

The following pull requests have modified the text or graphics of this chapter:
- none
