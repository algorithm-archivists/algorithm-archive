## What's a probability distribution?

It's easy to understand what a __discrete__ probability distribution is - for example, the probability distribution of obtaining the number $$n$$ in a dice roll is

$$
P(n) = \begin{cases}
			\frac 1 6 & n \in [1..6] \\
			0         & n \notin [1..6] 
		\end{cases}
$$

which is basically saying that the probability of $$n$$ being a whole number between 1 and 6 is $$1/6$$, and the probability of getting any other $$n$$ is 0. This is a discrete probability function because $$n$$ is an integer, and thus only takes discrete values. 

Another example of a discrete probability function is when $$n$$ is the sum of numbers on a roll of two die. In this case, $$P(n)$$  is different for each $$n$$ as some possibilities like $$n=2$$ can happen in only one possible way (by getting a 1 on both die), whereas $$n=4$$ can happen in 3 ways (1 and 3; or 2 and 2; or 3 and 1). So if we had to create a probability distribution for this system, we would first count up the number of possibilities for each $$n$$ - let's call this the frequency, $$f(n)$$ of each number. We can visualize $$f(n)$$ in a plot,

<p>
	<img class="center" src="res/die_roll.png" alt="<FIG> Die Roll" style="width:80%"/>
</p>

The above is NOT the probability $$P(n)$$ that we are after - because we know that the sum of all probabilities should be 1, which clearly isn't the case for $$f(n)$$. But we can just get that by dividing $$f(n)$$ by the _total_ number of possibilities, $$N$$. For two die, that is $$N = 6 \times 6 = 36$$, but we could also express it as the _sum of all frequencies_,

$$
N = \sum_n f(n)
$$

which would equal to 36 in this case. So, by dividing $$f(n)$$ by $$\sum_n f(n)$$ we get our target probability distribution, $$P(n)$$. This process is called __normalization__ and is crucial for determining almost any probability distribution. So in general, if we have the function $$f(n)$$, we can get the probability as

$$
P(n) = \frac{f(n)}{\displaystyle\sum_{n} f(n)}
$$

Note that $$f(n)$$ does not necessarily have to be the frequency of $$n$$ - it could really be any function which is _proportional_ to $$P(n)$$, and the above definition of $$P(n)$$ would still hold. And it's easy to check that the sum is now equal to 1, since

$$
\sum_n P(n) = \frac{\displaystyle\sum_{n}f(n)}{\displaystyle\sum_{n} f(n)} = 1
$$

Once we have the probability function $$P(n)$$, we can calculate all sorts of probabilites. For example, the probability that $$n$$ will be between two integers $$a$$ and $$b$$ inclusive is simply the sum of the probabilities for each value of $$n$$ in that range, i.e.,

$$
Probability(a \leq n \leq b) = \sum_{n=a}^{b} P(n)
$$

### A Probability Density Function

What if instead of a discrete variable $$n$$, we had a continuous one, like temperature or weight? In that case, it doesn't make sense to ask what the probability is of $$x$$ being _exactly_ a particular number - there are infinite possible real numbers, after all, so the probability of $$x$$ being exactly any one of them is essentially zero! But it _does_ make sense to ask what the probability is that $$x$$ will be between a certain range of values. For example, one might say that there is 50% chance that the temperature tomorrow noon will be between 5 and 15, or 5% chance that it will be between 16 and 16.5. But how do we put all that information, for every possible range, in a single function? The answer is to use a __probability density function__. 

 What does that mean? Well, suppose $$x$$ is a continous quantity, and we have a probability density function, $$P(x)$$ which looks like

<p>
	<img class="center" src="res/probability_density.png" alt="<FIG> probability density" style="width:80%"/>
</p>

Now imagine that the thin sliver in the diagram, placed at $$x=x_0$$ and with a width of $$dx$$, is really, really thin - infinitesimally thin, to be precise. In that case, the probability that the value of $$x$$ will be in the range $$ x_0 \lt x \lt x_0 + dx $$, is given by

$$
Probability(x_0 \leq x \leq x_0 + dx) = P(x)dx
$$

So strictly speaking, it is not $$P(x)$$ which is the probability, but rather the quantity $$P(x)dx$$. That is why we call $$P(x)$$ the probability density at $$x$$, and the actual probability is only defined for ranges of $$x$$. But what about for large ranges of $$x$$, which are not infinitesimal? We do exactly what we did for the discrete case - sum up all the individual probabilities for each possible values over the range of values. And what do we call a sum over a continuous variable? Why, an integral, of course! Who knew calculus would come in handy one day? And so we have,

$$
Probability(a \leq x \leq b ) = \int_a^b P(x)dx
$$

And the fact that all probabilities must sum to 1 translates to

$$
\int_D P(x)dx = 1
$$

where $$D$$ denotes the __domain__ of $$P(x)$$, i.e., the entire range of possible values of $$x$$ for which $$P(x)$$ is defined. 
 
## Normalization of a Density Function

Just like in the discrete case, we often first calculate some density or frequency function $$f(x)$$, which is NOT $$P(x)$$, but proportional to it. We can get the probability density function by normalizing it in a similar way, except that we integrate instead of sum:

$$
P(\mathbf{x}) = \frac{f(\mathbf{x})}{\int_D f(\mathbf{x})d\mathbf{x}}
$$

For example, consider the __normal distribution function__, 

$$
f(x) = e^{-x^2}
$$

which is defined for all real numbers $$x$$. We first integrate it (or do a quick google search) to get

$$
N = \int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}
$$

and so we have

$$
P(x) = \frac{1}{N} e^{-x^2} = \frac{1}{\sqrt{\pi}} e^{-x^2}
$$

which satisfies all properties of a probability density function (integrating to 1 and always positive).
