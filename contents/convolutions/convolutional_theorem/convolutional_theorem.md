# Convolutional Theorem

Important note: this particular section will be expanded upon after the Fourier transform and Fast Fourier Transform (FFT) chapters have been revised.

Now, let me tell you about a bit of computational magic:

**Convolutions can be performed with Fourier Transforms!**

This is crazy, but it is also incredibly hard to explain, so let me do my best.
As described in the chapter on [Fourier Transforms](../cooley_tukey/cooley_tukey.md), Fourier Transforms allow programmers to move from real space to frequency space.
When we transform a wave into frequency space, we can see a single peak in frequency space related to the frequency of that wave.
No matter what function we send into a Fourier Transform, the frequency-space image can be interpreted as a series of different waves with a specified frequency.
Each of these waves is parameterized by another $$e^{2\pi i k n / N}$$ term, where $$k$$ is the element's value in the frequency domain, $$n$$ is its value in the time domain, and $$N$$ is the overall length of the signal.
In this way, each wave can be seen as a complex exponential.

So here's the idea: if we take two functions $$f(x)$$ and $$g(x)$$ and move them to frequency space to be $$\hat f(\xi)$$ and $$\hat g(\xi)$$, we can then multiply those two functions and transform them back into to blend the signals together.
In this way, we will have a third function that relates the frequency-space images of the two input functions.
This is known as the *convolution theorem* which looks something like this:

$$\mathcal{F}(f*g) = \mathcal{F}(f) \cdot \mathcal{F}(g)$$

Where $$\mathcal{F}$$ denotes the Fourier Transform.

At first, this might not seem particularly intuitive, but remember that frequency space is essentially composed of a set of exponentials.
As mentioned in the section about [Multiplication as a Convolution](../multiplication/multiplication.md), multiplication in base 10 space is also a convolution.
The convolutional theorem extends this concept into multiplication with *any* set of exponentials, not just base 10.
Obviously, this description is still lacking a bit of explanation, but I promise we will add more when revising the Fourier transform sections!

By using a Fast Fourier Transform (FFT) in code, this can take a standard convolution on two arrays of length $$n$$, which is an $$\mathcal{O}(n^2)$$ process, to $$\mathcal{O}(n\log(n))$$.
This means that the convolution theorem is fundamental to creating fast convolutional methods for certain large inputs.

{% method %}
{% sample lang="jl" %}
[import:5-8, lang:"julia"](code/julia/convolutional_theorem.jl)
{% endmethod %}

This method also has the added advantage that it will *always output an array of the size of your signal*; however, if your signals are not of equal size, we need to pad the smaller signal with zeros.
Also note that the Fourier Transform is a periodic or cyclic operation, so there are no real edges in this method, instead the arrays "wrap around" to the other side, creating a cyclic convolution like we showed in the periodic boundary condition case for the [one-dimensional convolution](../1d/1d.md).

## Example Code

For this example code, we will be using two sawtooth functions as we did in the chapter on [one-dimensional convolutions](../1d/1d.md):

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/convolutional_theorem.jl)
{% sample lang="py" %}
[import, lang:"python"](code/python/convolutional_theorem.py)
{% endmethod %}

This should produce the following output:

<p>
    <img class="center" src="../res/cyclic.png" style="width:75%">
</p>

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/main/LICENSE.md)).

##### Images/Graphics

- The image "[Cyclic](../res/cyclic.png)" was created by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).


##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none

