# Convolutions
Alright, I am going to come right out and say it: convolutions can be confusing.
Not only are they hard to really describe, but if you do not see them in practice, it's hard to understand why you would ever want to use them.
I'm going to do what I can to describe them in an intuitive way; however, I may need to come back to this in the future.
Let me know if there is anything here that is unclear, and I'll do what I can to clear it up.

If you take two functions $$f(x)$$ and $$g(x)$$, there are a number of ways you can combine them.
All basic operations can do this (addition, subtraction, multiplication, and division), but there are also special operations that only work with functions and do not work on standard variables or numbers.
For example, $$f \circ g$$ is a *composition* of the two functions, where you plug $$g(x)$$ into $$f(x)$$.
A convolution is another function-related operation, and is often notated with a star ($$*$$) operator, where

$$f(x)*g(x)=c(x)$$

provides a third function $$c(x)$$ that blends $$f(x)$$ and $$g(x)$$.

As a rather important side-note: there is an incredibly similar operator known as a *correlation* which will be discussed in the near future.
For now, let's focus on convolutions, which are defined as:

$$(f*g)(x) = \int_{-\infty}^{\infty}f(\xi)g(x-\xi)d\xi = \int_{-\infty}^{\infty}f(x-\xi)g(\xi)d\xi$$

Note that in this case, $$x$$ is not necessarily a spatial element.
Often times, it is time or something else entirely!
The easiest way to think about this is that the function $$g(x)$$ is being shifted across all of space by the variable $$\xi$$.
At every point $$x$$, we multiply $$f(x)$$ and $$g(x)$$ and integrate the multiplied output to find the convolution for that spatial step, $$(f*g)(x)$$.
Note that in code, this is often discretized to look like:

$$(f*g)[n] = \sum_{m = -\infty}^{\infty}f[m]g[n-m] = \sum_{m = -\infty}^{\infty}f[n-m]g[m]$$

Where `f[n]` and `g[n]` are arrays of some form.
This means we basically just need to keep one array steady, flip the second array around, and move it through the first array one step at a time, performing a simple element-wise multiplication each step.

<!---This can be seen in the following animation:--->

<!---ADD ANIMATION--->

In code, this looks something like:

{% method %}
{% sample lang="jl" %}
[import:1-17, lang:"julia"](code/julia/conv.jl)
{% sample lang="hs" %}
[import:6-9, lang:"haskell"](code/haskell/convolution.hs)
{% sample lang="c"%}
[import:5-18, lang:"c_cpp"](code/c/convolutions.c)
{% sample lang="cpp"%}
[import:68-88, lang:"c_cpp"](code/c++/convolutions.cpp)
{% sample lang="python"%}
[import:4-19, lang:"python"](code/python/conv.py)
{% endmethod %}

Note that in this case, the output array will be the size of `f[n]` and `g[n]` put together.
Sometimes, though, we have an large size for `f[n]` and a small size for `g[n]`.
In this case `g[n]` is often called a *filter*, and often times when we are using a filter on an array (that might represent an image or some form of data), we want the output array to be the same size as the input.
In this case, rather than outputting a larger array, we often do something special at the borders of the array.
Depending on the situation, this may be necessary.
Note that there are different methods to deal with the edges in this case, so it's best to do whatever seems right when the situation arises.

### Convolutional Theorem

Now, let me tell you about a bit of black computational magic:

**Convolutions can be performed with Fourier Transforms!**

That is crazy!
It's also incredibly hard to explain, so let me do my best.
As described in the chapter on [Fourier Transforms](../cooley_tukey/cooley_tukey.md), Fourier Transforms allow programmers to move from real space to frequency space.
When we transform a wave into frequency space, we see a single peak in frequency space related to the frequency of that wave.
No matter what function we send into a Fourier Transform, the frequency-space image can be interpreted as a series of different waves with a specified frequency.

So here's the idea: if we take two functions $$f(x)$$ and $$g(x)$$ and move them to frequency space to be $$\hat f(\xi)$$ and $$\hat g(\xi)$$, we can then multiply those two functions and transform them back into a third function to blend the signals together.
In this way, we will have a third function that relates the frequency-space images of the two input functions.
*This is precisely a convolution!*

Don't believe me?
Well, this is because of something known as the *convolution theorem* which looks something like this:

$$\mathcal{F}(f*g) = \mathcal{F}(f) \cdot \mathcal{F}(g)$$

Where $$\mathcal{F}$$ denotes the Fourier Transform.
Now, by using a Fast Fourier Transform (fft) in code, this can take a standard convolution on two arrays of length $$n$$, which is an $$\mathcal{O}(n^2)$$ process, to $$\mathcal{O}(n\log(n))$$.
This means that the convolution theorem is fundamental to creating fast convolutional methods for large inputs, assuming that both of the input signals are similar sizes.
That said, it is debatable whether the convolution theorem will be faster when the filter size is small.
Also: depending on the language used, we might need to read in a separate library for FFT's.

{% method %}
{% sample lang="jl" %}
That said, Julia has an in-built fft routine, so the code for this method could not be simpler:
[import:19-22, lang:"julia"](code/julia/conv.jl)
Where the `.*` operator is an element-wise multiplication.
{% sample lang="hs" %}
[import:11-14, lang:"haskell"](code/haskell/convolution.hs)
Where the `.*` operator is an element-wise multiplication.
{% sample lang="c"%}
[import:20-30, lang:"c_cpp"](code/c/convolutions.c)
{% sample lang="cpp"%}
[import:90-105, lang:"c_cpp"](code/c++/convolutions.cpp)
{% sample lang="python"%}
[import:22-43, lang:"python"](code/python/conv.py)
{% endmethod %}

This method also has the added advantage that it will *always output an array of the size of your signal*; however, if your signals are not of equal size, we need to pad the smaller signal with zeros.
Also note that the Fourier Transform is a periodic or cyclical operation, so there are no real edges in this method, instead the arrays "wrap around" to the other side.
For this reason, this convolution is often called a *cyclic convolution* instead of a *linear convolution* like above.
Note that cyclic convolutions can definitely still be done without Fourier Transforms and we can do linear convolutions with Fourier Transforms, but it makes the code slightly more complicated than described above.

<!---
If you are still having trouble wrapping your head around what the convolution theorem actually means, maybe this graphic will help:

ADD IMAGE

Remember that each element of the frequency-space array is a different waveform in real-space, so when you multiply two frequency-space arrays, you are selectively amplifying similar waveforms.
--->

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
