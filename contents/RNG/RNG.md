# Random Number Generation

Quick, try to think of a number between 1 and 10.

Ok, do you have your number?

Was it 7?

Probably not, right? I mean, there is no magic here.
I put down 7 (seemingly at random), and you chose your number seemingly at random.
In general, there should be about a 10% chance that we chose the same number; however, that is statistically not the case.

For some reason, we naturally choose 7 more than any other integer. The exact probability of choosing 7 is currently up for debate, ranging from roughly 25% {{ "RNG_reddit" | cite }} to 45% {{ "RNG_Numberphile" | cite }}, and there are some interesting papers on related topics you might want to chek out as well {{ "Shepard197582" | cite }} {{ "navarro2008latent" | cite }}.

My point is that we, as humans are really bad at coming up with random numbers.
Computers, on the other hand, are some of the most sofisticated pieces of technology we have ever built.
They have sometimes trillions of transistors, all working to provide answers to fundamental questions in science and technology.
Surely they are good at creating random numbers, right?

Yeah, not really. Like, they are fine, but they are machines built by people whowant precise answers.
Randomness is not usually something people want when trying to add $$2+2$$.

So then what do we do?
Well, white noise (static) is truly random [CITE] and comes about all the time in nature, so we could just put a microphone somewhere and collect random numbers from the world around us.
The problem is that nature is kinda slow at doing this and we want really fast simulations, so it's not in our best interest to just chill by a pond fishing for numbers.
So instead let's come up with complex algorithms that *simulate* randomness as best as we can.
From here on, rather than talking about generating truly random numbers, we will discuss creating "random enough" *pseudo* random numbers.

In the literature, you might see Random Number Generation being shortened to RNG and Pseudo Random Number generation shortened to PRNG.
The two are often used interchangeable, but there is a small destinction to be made.
Namely that RNG is related to the entire process of generating random numbers, while PRNG is a subset of methods that use the computer to do so.
In certain fields, other terms are used, but these will be covered on a case-by-case basis.

In these chapters, we'll do our best to cover a bunch of different algorithms and their specific use-cases.
Before doing that, we'll just talk about 2 things:
1. Why we want randomness to begin with
2. How to test for randomness

## Why do we want random numbers?

Monte Carlo, simulating nature, etc

computer graphics / IFS

Cryptographic hashing

Note that we need different quality of RNG for this


## Testing for Randomness

Histogram test

### Bibliography

{% references %} {% endreferences %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

