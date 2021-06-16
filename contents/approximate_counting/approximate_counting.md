# The Approximate Counting Algorithm

This might seem like a straightforward question, but how high can you count on your fingers?
I mean, this depends on how many fingers you have, but in general the answer has to be 10, right?

Well, not exactly, it can actually go much, much higher with a few simple abstractions.

The first strategy is to think of your fingers as binary registers, like so {{ "3b1b_finger_count" | cite }}:

<p>
    <img  class="center" src="res/hands.png" style="width:70%" />
</p>

If your fingers are out, they count as a 1 for that register.
If they are not, they count as a 0 for that register.
After you have decided on the appropriate finger configuration, you have created a bitstring that can be read from left to right, where each number represents a power of 2.
For this example, we would have a bitstring of 1110010101, which reads to 917:

$$
1 \cdot 2^9 + 1 \cdot 2^8 + 1 \cdot 2^7 + 0 \cdot 2^6 + 0 \cdot 2^5 + 1 \cdot 2*4 + 0 \cdot 2^3 + 1 \cdot 2^2 + 0 \cdot 2^1 + 1 \cdot 2^0
$$
$$
=
$$

$$
512 + 256 + 128 + 16 + 4 + 1 = 917
$$

Because you have 10 fingers and each one represents a power of 2, you can count up to a maximum of $$2^{10}$$ or 1023, which is about 100 times more than simple finger counting!

This is great, but what if we wanted to go beyond 1024?
More concretely: how high can we go with only 10 bits?

This is precisely the problem that Morris encountered in Bell Labs around 1977 {{"morris1978counting" | cite }}.
There, he was given an 8-bit register and asked to count much higher than $$2^8 = 256$$.
His solution was to invent a new method known as the approximate counting algorithm.
With this method, he could count to about $$130,000$$ with a relatively low error (standard deviation, $$\sigma \approx 17,000$$).
Using 10 registers (fingers), this method can count to about $$1.15\times 10^{16}$$ when using similar parameters, which is undoubtedly impressive!

This method is an early predecessor to streaming algorithms where information must be roughly processed in real-time.
As we dive into those methods later, this chapter will certainly be updated.
For now, we will not be showing any proofs (though those might come later as well), but a rigorous mathematical description of this method can be found in a follow-up paper by Philippe Flajolet {{ "flajolet1985approximate" | cite }}.
In addition, there are several blogs and resources online that cover the method to varying degrees of accessibility {{"arpit_counting" | cite }} {{"greggunderson_counting" | cite }}.

Here, we hope to provide a basic understanding of the method, along with code implementations for anyone who might want to try something similar in the future.

## A Simple Example

If we need to count more than 256 events with 8 bits, there is one somewhat simple strategy: just count every other item.
This means that we will increment our counter with 2, 4, 6, 8... events, effectively doubling the number of events we can count to 512!
Similarly, if we need to count above 512, we can increment our counter every 3 or 4 events; however, the obvious drawback to this method is that if we only count every other event, there is no way to represent odd numbers.
Similarly, if we count every 3rd or 4th event, you would miss out on any numbers that are not multiples of your increment number.

As a hypothetical example, imagine counting 1,000,000 sheep, if we wanted to save all of them on 8 bits (maximum size of 256), we could increment our counter every ~4000 sheep.
By counting in this way, we would first need to count 4000 sheep before incrementing the main counter by 1.
After all the sheep have gone by, we would have counted up to 250 on our counter, and also counted up to 4000 on a separate counter 250 times.
This has a few important consequences:
1. If the final number of sheep is not a multiple of 4000, then we will have an error associated with the total count of up to 4000 (0.4%).
2. There is no way to determine the final number of sheep if it is not a multiple of 4000.
3. We now need some way to count up to 4000 before incrementing the main counter!

In this way, 4000 would be a type of "counting resolution" for our system.
Overall, a 0.4% error is not bad, but it is possible to ensure that the approximate count is more accurate by using randomness to our advantage.

That is to say, instead of incrementing out counter every 4000th sheep, we could instead give each item a 0.025% chance of incrementing our main counter.
This averages out to be roughly 1 count every 4000 sheep, but the expectation value of a large number of counting experiments should be the correct number.
This means that even though we need to count all the sheep multiple times to get the right expectation value, we no longer need to keep a separate counter for the counting resolution of 4000.

As expected, each counting experiment will have some associated error (sometimes much higher than 0.4%).
To quantify this error, let's actually perform the experiment

ADD image of counting to 1,000,000

For this image, we have performed _____ counting experiments and have also plotted a Gaussian distribution on top of it.
This distribution is known as a Binomial distribution, which we'll cover in the near future.
For now, it is alright to assume that it is *close enough* to a Gaussian to use Gaussian approximations.
This means that we can estimate the error with standard deviations and find the final count with the expectation value of the distribution.
Here, the expectation value is _____ and the standard deviation is ____.

As an interesting side-note, let's plot a few experiments alongside a counting line $$y=x$$ (which represents simply incrementing by 1 each item instead of in steps of 4000 at a 0.024% chance).

ADD image tracking the total count of different counting experiments with y=x.

Here, it is clear that by averaging all of the distributions, we roughly get the same $$y=x$$ counting line, but none of the experiments seem to be particularly close to the true count without averaging them together.
We have also shown snapshots of all distributions at 1000, 500,000 and 1,000,000 to show how good this approximation is at different stages of the experiment.
If we are only interested in an order-of-magnitude estimate, the average does the job quite well when we have counted 1,000,000 objects; however, for smaller counts, like 1000, the estimate is harder to justify.

For this reason, Morris introduced logarithmic counting, which should allow us to more closely track the counting line and find a better order-of-magnitude estimate at all scales.

## Adding a logarithm

At this stage, we will begin to talk about events events, which are a general abstraction to the sheep previous sheep analogy.
We will also introduce three different values:

* $$n$$: the number of events that have occurred.
* $$v(n)$$: the approximate count we have stored in our bitstring.
* 


Histogram + tracking line images

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
