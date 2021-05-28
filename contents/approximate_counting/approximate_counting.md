# The Approximate Counting Algorithm

This might seem like a straightforward question, but how high can you count on your fingers?
I mean, some people might have more or less fingers, but in general the answer has to be 10, right?

Well, not exactly, it can actually go much, much higher with a few simple abstractions.

The first strategy is to think of your fingers as binary registers, like so:

ADD IMAGE of different hand configurations showing different binary numbers.

If your fingers are out, they count as a 1 for that register.
If they are not, they count as a 0 for that register.
After you have decided on the appropriate finger configuration, you can just read them back to get your actual count.
Because you have 10 fingers and each one represents a power of 2, you can count up to $$2^10$$ or 1023, which is 100 times more than simple finger counting!

This is great, but what if we wanted to go beyond 1024?
How high can we go with only 10 bits?

This is precisely problem that Morris encountered in Bell Labs around 1977 [CITE].
There, he was given an 8-bit register and asked to count much higher than $$2^8 = 256$$.

His solution was to invent a new algorithm known as the approximate counting algorithm.

## A Simple Example

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
