# Gaussian Elimination

So, how exactly do we go about solving a system of linear equations? Well, one way is _Gaussian Elimination_, which you may have encountered before in a math class or two. The basic idea is that we take a system of equations,


$$
\begin{align}
2x + 3y + 4z &= 6 \\
x + 2y + 3z &= 4 \\
3x - 4y &= 10
\end{align}
$$


and turn it into a matrix by using the coefficients in front of each variable


$$
\left[
\begin{array}{ccc}
2 & 3  & 4\\
1 & 2 & 3\\
3 & -4 & 0
\end{array}
\right]
\left[
\begin{array}{c}
x \\
y \\
z
\end{array}
\right]
=
\left[
\begin{array}{c}
6 \\
4 \\
10
\end{array}
\right]
$$


Or more simply:


$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
3 & -4 & 0 & 10
\end{array}
\right]
$$


Now, at first, this doesn't seem to help anything, so let's think of this in another way. Wouldn't it be great if the system of equations looked like this:


$$
\begin{align}
2x + 3y + 4z &= 6 \\
y + 2z &= 2 \\
11z &= 18
\end{align}
$$


Then we could just solve for $$z$$ and plug that value in to the top two equations to solve for $$x$$ and $$y$$! In matrix form, it would look like this


$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18
\end{array}
\right]
$$


and it has a particular name: _Row Eschelon Form_. Basically, any matrix can be considered in row eschelon form if

1. All non-zero rows are above rows of all zeros
2. The leading coefficient or _pivot_ (the first non-zero element in every row when reading from left to right) is right of the pivot of the row above it.

Now, Row Eschelon Form is nice, but wouldn't it be even better if our system of equations looked simply like this


$$
\begin{align}
11x &= 18 \\
11y &= -14 \\
11z &= 18
\end{align}
$$


Then we would know exactly what $$x$$, $$y$$, and $$z$$ are without any fuss! In matrix form, it looks like


$$
\left[
\begin{array}{ccc|c}
11 & 0 & 0 & 18 \\
0 & 11 & 0 & -14 \\
0 & 0 & 11 & 18
\end{array}
\right]
$$


And again has a special name * **Reduced** Row Eschelon Form*. Now, it seems obvious to point out that if we remove the values to the right of the equals sign \($$=$$\), Row Eschelon Form is an upper triangular matrix, while Reduced Row Eschelon Form is diagonal. This might not be important now, but it will play an important role in future discussions, so keep it buzzing in the back of your brain.

For now, I hope the motivation is clear: we want to convert a matrix into Row Eschelon and (potentially) Reduced Row Eschelon Form to make large systems of equations trivial to solve, so we need some method to do that. What is that method called? \(Hint: It's the title of this section\)

That's right! _Gaussian Elimination_

## The Method

Here I should point out that Gaussian elimination makes sense from a purely analytical point of view. That is to say that for small systems of equations, it's relatively straightforward to do this method by hand; however, for large systems, this \(of course\) become tedious and we will need to find an appropriate numerical solution. For this reason, I have split this section into two parts. One will cover the analytical framework, and the other will cover an algorithm you can write in your favorite programming language.

In the end, reducing large systems of equations boils down to a game you play on a seemingly random matrix where you have the following moves available:

1. You can swap any two rows
2. You can multiply any row by a non-zero scale value
3. You can add any row to a multiple of any other row

That's it. Before continuing, I suggest you try to recreate the Row Eschelon matrix we made above. That is, do the following:

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
3 & -4 & 0 & 10
\end{array}
\right]
\quad \rightarrow \quad
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18
\end{array}
\right]
$$

There are plenty of different strategies you could use to do this, and no one strategy is better than the rest. Personally, I usually try to multiply each row in the matrix by different values and add rows together until the first column is all the same value, and then I subtract the first row from all subsequent rows. I then do the same thing for the following columns.

After you get an upper triangular matrix, the next step is diagonalizing to create the Reduced Row Eschelon Form. In other words, we do the following:

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18
\end{array}
\right]
\quad \rightarrow \quad
\left[
\begin{array}{ccc|c}
11 & 0 & 0 & 18 \\
0 & 11 & 0 & -14 \\
0 & 0 & 11 & 18
\end{array}
\right]
$$

Here, the idea is similar to above. You can do basically anything you want. My strategy is usually the same as before, but starts from the right-most column and subtracts upwards instead of downwards.

## The Algorithm

Now, the analytical method may seem straightforward, but the algorithm does not obviously follow from the game we were playing before, so we'll go through it step-by-step.

In general, do the following process:

1. For each column `col`, find the highest value
$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
\mathbf{3} & -4 & 0 & 10
\end{array}
\right]
$$
2. Swap the row with the highest valued element with the `col`th row.
$$
\left[
\begin{array}{ccc|c}
\mathbf{2} & \mathbf{3}  & \mathbf{4} & \mathbf{6} \\
1 & 2 & 3 & 4 \\
\mathbf{3} & \mathbf{-4} & \mathbf{0} & \mathbf{10}
\end{array}
\right]
\rightarrow
\left[
\begin{array}{ccc|c}
\mathbf{3} & \mathbf{-4} & \mathbf{0} & \mathbf{10} \\
1 & 2 & 3 & 4 \\
\mathbf{2} & \mathbf{3}  & \mathbf{4} & \mathbf{6}
\end{array}
\right]
$$
3. For all remaining rows, find a fraction that corresponds to the ratio of the lower value in that column to the central pivot \(the one you swapped to the top\)
$$
\rightarrow
\left[
\begin{array}{ccc|c}
3 & -4 & 0 & 10 \\
\mathbf{1} & 2 & 3 & 4 \\
2 & 3  & 4 & 6
\end{array}
\right] \\
\begin{align}
    f &= A(\text{pivot}_{\text{row}}, \text{pivot}_{\text{col}}) / A(\text{curr_row}_{\text{row}}, \text{pivot}_{\text{col}}) \\
      &= \frac{1}{3}
\end{align}
$$
4. Set all values in the corresponding rows to be the value they were before $$-$$ the top row $$\times$$ the fraction. This is essentially performing move 3 from above, except with an optimal multiplicative factor.
$$
A(\text{curr_row}_{\text{row}}, \text{curr_col}_{\text{col}}) \mathrel{+}= A(\text{pivot_row}_{\text{row}}, \text{pivot_row}_{\text{curr_col}} \times f) \\
\left[
\begin{array}{ccc|c}
3 & -4 & 0 & 10 \\
\mathbf{1} & \mathbf{2} & \mathbf{3} & \mathbf{4} \\
2 & 3  & 4 & 6
\end{array}
\right]
\rightarrow
\left[
\begin{array}{ccc|c}
3 & -4 & 0 & 10 \\
\mathbf{\frac{1}{3}} & \mathbf{\frac{2}{3}} & \mathbf{1} & \mathbf{\frac{4}{3}} \\
2 & 3  & 4 & 6
\end{array}
\right]
$$
5. Set the value of that row's pivot column to 0.
$$
\left[
\begin{array}{ccc|c}
3 & -4 & 0 & 10 \\
0 & 2 & 3 & 4 \\
2 & 3  & 4 & 6
\end{array}
\right]

$$

In code, this looks like:

{% method %}
{% sample lang="jl" %}
[import:1-42, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:13-44, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% endmethod %}

As with all code, it takes time to fully absorb what is going on and why everything is happening; however, I have tried to comment the above psuedocode with the necessary steps. Let me know if anything is unclear!

Now, to be clear: this algorithm creates an upper-triangular matrix. In other words, it only creates a matrix in *Row Eschelon Form*, not * **Reduced** Row Eschelon Form*! So what do we do from here? Well, we could create another step to further reduce the matrix, but another method would be to use *Back-Substitution*.

The back-substitution method is precisely what we said above.
If we have a matrix in Row-Eschelon Form, we can directly solve for $$z$$, and then plug that value in to find $$y$$ and then plug both of those values in to find $$x$$!
Even though this seems straightforward, the pseudocode might not be as simple as you thought!

{% method %}
{% sample lang="jl" %}
[import:44-64, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:46-58, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% endmethod %}

Now, as for what's next... Well, we are in for a treat! The above algorithm clearly has 3 `for` loops, and will thus have a complexity of $$\sim O(n^3)$$, which is abysmal! If we can reduce the matrix to a specifically **tridiagonal** matrix, we can actually solve the system in $$\sim O(n)$$! How? Well, we can use an algorithm known as the _Tri-Diagonal Matrix Algorithm_ \(TDMA\) also known as the _Thomas Algorithm_.

### Example Code

The full code can be seen here:

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% endmethod %}


<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
$$
\newcommand{\d}{\mathrm{d}}
\newcommand{\bff}{\boldsymbol{f}}
\newcommand{\bfg}{\boldsymbol{g}}
\newcommand{\bfp}{\boldsymbol{p}}
\newcommand{\bfq}{\boldsymbol{q}}
\newcommand{\bfx}{\boldsymbol{x}}
\newcommand{\bfu}{\boldsymbol{u}}
\newcommand{\bfv}{\boldsymbol{v}}
\newcommand{\bfA}{\boldsymbol{A}}
\newcommand{\bfB}{\boldsymbol{B}}
\newcommand{\bfC}{\boldsymbol{C}}
\newcommand{\bfM}{\boldsymbol{M}}
\newcommand{\bfJ}{\boldsymbol{J}}
\newcommand{\bfR}{\boldsymbol{R}}
\newcommand{\bfT}{\boldsymbol{T}}
\newcommand{\bfomega}{\boldsymbol{\omega}}
\newcommand{\bftau}{\boldsymbol{\tau}}
$$

