# The Approximate Counting Algorithm

This might seem like a straightforward question, but how high can you count on your fingers?
I mean, some people might have more or less fingers, but in general the answer has to be 10, right?

Well, not exactly, it can actually go much, much higher with a few simple abstractions.

The first strategy is to think of your fingers as binary registers, like so:

ADD IMAGE of different hand configurations showing different binary numbers.

If your fingers are out, they count as a 1 for that register.
If they are not, they count as a 0 for that register.
After you have decided on the appropriate finger configuration, you can just read them back to get your actual count.
Because you have 10 fingers and each one represents a power of 2, you can count up to $$2^{10}$$ or 1023, which is 100 times more than simple finger counting!

This is great, but what if we wanted to go beyond 1024?
How high can we go with only 10 bits?

This is precisely problem that Morris encountered in Bell Labs around 1977 [CITE].
There, he was given an 8-bit register and asked to count much higher than $$2^8 = 256$$.

His solution was to invent a new algorithm known as the approximate counting algorithm.
With this method, he could count to about $$130,000$$ with a relatively low error (standard deviation, $$\sigma \approx 17,000$$).
Using 10 registers (fingers), this method can count to about $$1.15\times 10^{16}$$ when using similar parameters, which is undoubtedly impressive!

This method has was an early predecessor to streaming algorithms where information must be roughly processed in real-time.
As we dive into those methods later, this chapter will certainly be updated.
For the purposes of this chapter, we will not be showing any proofs (though those might come later), but a rigorous mathematical description of this method can be found in the original paper [CITE], or a follow-up paper by ... [CITE].
In addition, there are several blogs and resources online that cover the method to varying degrees.

Here, we hope to provide a basic understanding of the method, along with code implementations for anyone who might want to try something similar in the future.

## A Simple Example

If we need to count more than 256 events in 8 bits, there is one somewhat simple strategy: just count every other item.
This means that we will increment our counter with 2, 4, 6, 8... events, effectively doubling the number of events we can count!
Similarly, if we need to count above 512, we can count every 3 or 4 events; however, there is an obvious drawback to this method.

Let's say we are counting every other event, but there are an odd number of events total.
Well, there would be no way to represent this number with that counting method.
Similarly, if you count every 3rd or 4th event, you would miss out on any numbers that are not multiples of your increment number.

Hypothetically, if you needed to count to some number around 1,000,000 with 8 bits, then you would need to count every ~4000 events.
This has a few important consequences:
1. If your number is not a multiple of 4000, then you will have an error associated with your count of up to 4000 (0.4%).
2. There is no way to determine the final count if it is not a multiple of 4000.
3. We now need some way to count up to 4000 before incrementing the main counter!

In ths way, 4000 would be a type of "counting resolution" for your system.
Overall, this error is not bad, but it is possible to ensure that the approximate count is more specific by using some random numbers.

That is to say, instead of counting every 4000th item, we could instead give each item a 0.025% chance of incrementing our counter.
This averages out to be roughly 1 count every 4000 items, but the expectation value of a large number of counting experiments will be the correct number.
Each counting experiment will have some associ


## Adding a logarithm

## Video Explanation

Here is a video describing the Barnsley fern:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/xoXe0AljUMA"
 frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; pic
ture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/barnsley.jl)
{% endmethod %}

### Bibliography

{% references %} {% endreferences %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

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
