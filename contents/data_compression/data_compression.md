# Data Compression

I'll be honest, data compression used to be an incredibly hot topic, but kids these days usually have incredibly powerful devices that do not seem to be memory-limited at all.
Because of this, it seems odd to talk about data compression as a field of intense debate and development.
It would naively seem that better hardware means that there are less restrictions on programmers and less of a need to search for new and unique ways to compress their data; however, this is far from the case.

That said, there will always be new devices on the market that require minimizing data storage.
In fact, some of the most revolutionary algorithms and methods in existence today fall in the category of data compression.
From lossless data compression with [Huffman encoding](../huffman_encoding/huffman_encoding.md) to genetic compression algorithms and machine learning, there is a lot to learn about this field, and we'll go through it piece-by-piece.

All that said, no discussion about data compression is complete without first discussing the information, itself -- specifically how information is represented in computer systems.
Now, we've discussed this in some depth before with [bitlogic](../../principles_of_code/building_blocks/bitlogic.md), but there is much more to the story than what we let on before.
Let's start with a working definition of information:

*Information is a representation of certainty.*

This might seem like a silly, hand-wavey definition, but hear me out.
If I am uncertain about something, I will ask a question.
The answer to this question could be any number of things, but it will contain information from an individual with some level of certainty.
For example, let's pretend you have been furiously coding for months in your mother's basement (it happens).
At some point, you realize you haven't gone outside and are completely unaware of the day, month, or even year!
As your mother slips food under your door one day, you reach out to her and ask, "What's it like outside today?"
Overjoyed at the prospect of her child finally leaving their room, your mother might say, "It's bright, sunny and warm. A perfect day to go outside and relax!"
This provides you a lot of information, and to some degree of certainty you can conclude it is summer.

Of course, after this interaction you do not acknowledge your mother's existence after her answer and go back to coding.
You have the information you need.
No reason to overcomplicate things with further human contact.

See, information is defined in a number of different places for a number of different reasons, so for our purposes, we will define the unit of information to be the *bit*, a simple binary 1 or 0.
Taking the example mentioned before (assuming you can take your mother at face-value), you were provided 3 true statements:

1. It is bright
2. It is sunny
3. It is warm

With this information, you assumed that it was probably summer.
Unfortunately, your assumption about it being summer is not information.
It might be a logical conclusion, but it was not provided as a "fact."
This is an important distinction between what we might colloquially describe as information and what information theory requires.
Information theory works with measurements -- binary absolutes.

Now, we can clearly say that *with some probability* it is summer, but this is a different story altogether, which we will undoubtedly discuss in the future.
For now, let's talk about a simple representation of information on computer systems.
Imagine you have a simple alphabet with only 2 characters in it, _a_ and _b_.
In this case, there are plenty of ways you can represent these characters in bits, but the most obvious way might look like this:

| Character | Bit Representation |
| --------- | ------------------ |
| _a_       | 0                  |
| _b_       | 1                  |

So long as you don't add any new characters to the mix, this is a perfectly valid set of codewords.
If you get the bitstring 0111101, you can easily decode it as _abbbbab_.

But what if you wanted to add a third character, _c_?
Well, it's clear that _c_ cannot be either 0 or 1, but because of the way we have defined the set of codewords above, it actually cannot be *any combination* of 0 or 1 either.
For example, if we defined _c_ to be 01 and we were provided the bitstring 0111101, we could interpret this string as either _abbbbab_ or _cbbbc_!
Now, we could use context or other information provided to distinguish these two possible cases, but it is clear that we need to think more deeply about our set of codewords in this case.

First, let's think a bit about decoding.
For our purposes, we do not want to think when decoding.
No matter what the bitstring is that we need to decode, we want to be able to read bit-by-bit until we find a match in our set of codewords and move on.
Basically, we do not want any ambiguity in our set of codewords.
The code for _c_ should not contain the code for _a_ or _b_!
In this way, our set of codewords should be *prefix-free*.
No word should appear as a prefix to another word.

If we wanted a good, prefix-free set of codewords for 4 characters (_a_, _b_, _c_, and _d_), it might look like this:

| Character | Bit Representation |
| --------- | ------------------ |
| _a_       | 00                 |
| _b_       | 01                 |
| _c_       | 10                 |
| _d_       | 11                 |

We can decode any even bitstring with this set.
0100101010110100 is _bacccdba_.
This is great, but now we have another question: given a string of characters, can we construct a set of codewords that *minimizes* the number of bits in its corresponding bitstring?

*This is the heart of data compression!*

First things first, let's define a simple measure for how compressed the data is.
Let's take the following set of characters: _abbcccdddd_.
If we were to put all the letters in a bag and pull one out at random, we would have the following probabilities of pulling out any of the letters:

| Character | Probability |
| --------- | ----------- |
| _a_       | .1          |
| _b_       | .2          |
| _c_       | .3          |
| _d_       | .4          |

This basically means that we are far more likely to pull out a _d_ than an _a_, and if we are trying to minimize the length of our encoded bitstring, the length of the bit representation for _d_ should probably be shorter than the bit representation for _a_.
Ultimately, to compress our encoded bitstring, we want to minimize the following quantity:

$$
L(C(W)) = \sum_{i=0}^{n}w_i\times \text{length}(c_i)
$$

Where $$C$$ is the characters of our alphabet and $$c_i$$ is codeword for a single character, $$W$$ is related to the probability of pulling that character out of a bag and $$w_i$$ is an individual probability of getting a specific character, and $$L$$ is a vague, unitless quantity to determine the overall length and is usually used to determine compression ratios.
To show how this works, let's consider encoding the word _abbcccdddd_ with two different alphabets:

| Character |  Probability | Bit Representation 1 | Bit Representation 2 |
| --------- | ------------ | -------------------- | -------------------- |
| _a_       | .1           | 00                   | 000                  |
| _b_       | .2           | 01                   | 001                  |
| _c_       | .3           | 10                   | 01                   |
| _d_       | .4           | 11                   | 1                    |

In this case:

$$
\begin{align}
L_1 &= 0.1\times 2 + 0.2 \times 2 + 0.3 \times 2 + 0.4 \times 2 = 2 \\
L_2 &= 0.1\times 3 + 0.2 \times 3 + 0.3 \times 2 + 0.4 \times 1 = 1.9
\end{align}
$$

Here, it's clear that $$L_2 < L_1$$, and thus the second set of codewords compresses our data more than the first.
This measure can be used as a direct test of certain simple data compression techniques, notably those created by Shannon, Fano, and [Huffman](../huffman_encoding/huffman_encoding.md), which will be covered soon!


<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
