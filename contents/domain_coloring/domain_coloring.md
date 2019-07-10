# Domain coloring

Domain coloring is a much more complicated plotting technique than those outlined above and is used to plot complex functions where both the input and output have imaginary and real components.
Imagine the following function:

$$f(z) = z^2$$

In this case, we could create a plot that looks like this:

ADD IMAGE

This indicated that for various input values along $$z$$, we have different function outputs from $$f(z)$$.

Unfortunately, complex variables and functions cannot be easily plotted in this way because the input variable and the output of $$f(z)$$ both need a two-dimensional space for plotting -- one axis for their real component and another for their imaginary component.

Now let's imagine another function:

$$f(z) = |z|$$

In this case, each complex input has a real output.
This can be plotted as a two-dimensional dataset like so:

Here, the $$x$$-axis and $$y$$-axis represent the real and imaginary components of the input variable respectively.

Here we see the problem.
If the output also requires plotting of the real and imaginary components, we need four dimensions to appropriately represent complex functions!
Unfortunately, feeble human minds are incapable of understanding four spatial dimensions, so we need to imoprovise.

We do this by assuming the complex output can be represented in the following form:

$$z = re^{i \theta} = r(\cos(\theta) + i\sin(\theta)$$

where, $$r$$ is a complex magnitude and $$\theta$$ is a complex phase.
This is the formula for a circle in the complex plane and we can easily find $$r$$ and $$\theta$$ like so:

$$
\begin{align}
    r &= \sqrt{\text{real}(z)^2 + \text{imag}(z)^2} \\
    \theta &= 
\end{align}
$$

Where $$\text{real}(z)$$ and $$\text{image}(z)$$ represent the real and imaginary components of $$z$$.

Once we have our complex function output in this form, we then color the output domain according to a number space with more than 2 dimensions -- Often HSV or HSL.

THESE WORK LIKE SO

So if we are using HSV, then we set the color values for each pixel in our image like so:

$$
\begin{align}
    H &= \\
    S &= \\
    V &= 100\%
\end{align}
$$

### Algorithms using this method:
- Split operator method

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Images/Graphics

##### Pull Requests

The following pull requests have modified the text or graphics of this chapter:
- none
