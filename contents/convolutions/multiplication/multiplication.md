# Multiplication as a convolution

As a brief aside, we will touch on a rather interesting side topic: the relation between integer multiplication and convolutions
As an example, let us consider the following multiplication: $$123 \times 456 = 56088$$.

In this case, we might line up the numbers, like so:

$$
\begin{matrix}
&&1&2&3 \\
&\times &4&5&6 \\
\hline
5 & 6 & 0 & 8 & 8
\end{matrix}
$$

Here, each column represents another power of 10, such that in the number 123, there is 1 100, 2 10s, and 3 1s.
So let us use a similar notation to perform the convolution, by reversing the second set of numbers and moving it to the right, performing an element-wise multiplication at each step:

$$
\begin{matrix}
&&&\color{red}1&2&3 \\
\times &6&5&\color{red}4&& \\
\hline
\end{matrix}\\
\color{red}{1}\times\color{red}{4} = 4
$$

$$
\begin{matrix}
&&&\color{red}1&\color{green}2&3 \\
\times &&6&\color{red}5&\color{green}4& \\
\hline
\end{matrix}\\
\color{red}1\times\color{red}5+\color{green}2\times\color{green}4=13
$$

$$
\begin{matrix}
&&&\color{red}1&\color{green}2&\color{blue}3 \\
\times &&&\color{red}6&\color{green}5&\color{blue}4 \\
\hline
\end{matrix}\\
\color{red}1\times\color{red}6+\color{green}2\times\color{green}5+\color{blue}3\times\color{blue}4=28
$$

$$
\begin{matrix}
&&1&\color{green}2&\color{blue}3& \\
\times &&&\color{green}6&\color{blue}5&4 \\
\hline
\end{matrix}\\
\color{green}2\times\color{green}6+\color{blue}3\times\color{blue}5=27
$$

$$
\begin{matrix}
&1&2&\color{blue}3&& \\
\times &&&\color{blue}6&5&4 \\
\hline
\end{matrix}\\
\color{blue}3\times\color{blue}6=18
$$

For these operations, any blank space should be considered a $$0$$.
In the end, we will have a new set of numbers:

$$
\begin{matrix}
&&1&2&3 \\
&\times &4&5&6 \\
\hline
4 & 13 & 28 & 27 & 18
\end{matrix}
$$

Now all that is left is to perform the *carrying* operation by moving any number in the 10s digit to its left-bound neighbor.
For example, the numbers $$[4, 18]=[4+1, 8]=[5,8]$$ or 58.
For these numbers, 

$$
\begin{matrix}
&4 & 13 & 28 & 27 & 18\\
=&4+1 & 3+2 & 8+2 & 7+1 & 8\\
=&5 & 5 & 10 & 8 & 8\\
=&5 & 5+1 & 0 & 8 & 8\\
=&5 & 6 & 0 & 8 & 8
\end{matrix}
$$

Which give us $$123\times456=56088$$, the correct answer for integer multiplication.
I am not suggesting that we teach elementary school students to learn convolutions, but I do feel this is an interesting fact that most people do not know: integer multiplication can be performed with a convolution.

This will be discussed in further detail when we talk about the Schonhage-Strassen algorithm, which uses this fact to perform multiplications for incredibly large integers.

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none

