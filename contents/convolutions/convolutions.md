# Convolutions
To put it bluntly, convolutions can be confusing.
Some might even call them *convoluted*!
(Get it? Because we are talking about *convolutions*? A wise man once told me that all good jokes need additional clarification.)

Not only are convolutions hard to describe, but if they are not used in practice, it is hard to understand why they would ever be needed.
I am going to do what I can to describe them in an intuitive way; however, I may need to come back to this in the future.
Let me know if there is anything here that is unclear, and I will do what I can to clear it up.

As always, we should start at the start.
If you take two functions $$f$$ and $$g$$, there are a number of ways you can combine them.
All basic operations can do this (addition, subtraction, multiplication, and division), but there are also special operations that only work with functions and do not work on standard variables or numbers.
For example, $$f \circ g$$ is a *composition* of the two functions, where you plug $$g(x)$$ into $$f$$.
A convolution is another function-related operation, and is often notated with a star $$(*)$$ operator, where

$$
f*g=c
$$

provides a third function, $$c$$, that is a blended version of $$f$$ and $$g$$.
As a rather important side-note: there is an incredibly similar operation known as a *correlation* which will be discussed in the near future.
Now we are left with a rather vague question: how do we *blend* functions?

To answer this question, we will need to show off a few simple graphics and animations in the [Convolutions in 1D](1d/1d.md) section while also discussing the mathematical definition of convolutions.
After, there will be a brief discussion on an interesting application of one dimensional convolutions in integer multiplication in the [Multiplication as a Convolution](multiplication/multiplication.md) section.
We will then move on to the most stereotypical application of convolutions in the [Convolutions of Images](2d/2d.md) section, where we will also discuss two important filters: the Gaussian kernel and the Sobel operator.
As a note: convolutions can be extended to $$n$$-dimensions, but after seeing how they are extended to two dimensions, it should be possible for the reader to extend it to three dimensions and beyond if that is needed, so we will not cover that in great detail here unless is is useful for another algorithm.
In addition, we will be touching on a rather difficult but powerful topic with the [Convolutional Theorem](convolutional_theorem/convolutional_theorem.md) section where convolutions can be computed by using [Fourier transforms](../Cooley_tukey/cooley_tukey.md).

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none

