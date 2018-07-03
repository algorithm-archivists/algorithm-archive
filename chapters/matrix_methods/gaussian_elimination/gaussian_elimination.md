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

Remember this one, we'll come back to it. Now, this is nice, but wouldn't it be even better if our system of equations looked simply like this


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

And that's where we really want to get to for obvious reasons.

Remember the earlier matrix form? It has a particular name: _Row Echelon Form_. Basically, any matrix can be considered in row echelon form if

1. All non-zero rows are above rows of all zeros
2. The leading coefficient or _pivot_ (the first non-zero element in every row when reading from left to right) is right of the pivot of the row above it.

All the following examples are in the Row Echelon Form:

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18
\end{array}
\right]
\;,\;
\left[
\begin{array}{ccc|c}
5 & 4  & 0 & 10 \\
0 & 0 & 5 & 7 \\
0 & 0 & 0 & 1
\end{array}
\right]
\;,\;
\left[
\begin{array}{ccccc}
1 & -3 & 4 & 1 & 6 \\
0 & 3 & 3 & 5 & 0 \\
0 & 0 & 0 & 2 & 0
\end{array}
\right]
\;,\;
\left[
\begin{array}{cc}
0 & 0 \\
0 & 0 \\
0 & 0
\end{array}
\right]
$$

Out of all of these, only the first two one make sense if you're talking about a system of linear equations, as the last two don't even have the right dimensions. Additionally, if you translate the last row of second matrix into a system, you get $$0=1$$, which is a contradiction. This is due to the fact that the matrix is singular, and there are no solutions to this particular system. Nevertheless, all of these are in Row Echelon Form.

Now, it seems obvious to point out that if we ignore the last column, Row Echelon Form is an upper triangular matrix. This might not be important now, but it will play an important role in future discussions, so keep it buzzing in the back of your brain.

As we discussed before, Row Echelon Form is not the terminus. Cue the * **Reduced** Row Echelon Form*.  A matrix is in Reduced Row Echelon form if it satisfies the following conditions:

1. It is in row echelon form.
2. Every pivot is 1 and is the only nonzero entry in its column.

All the following examples are in the Reduced Row Echelon Form:

$$
\left[
\begin{array}{ccc|c}
1 & 0 & 0 & 8 \\
0 & 1 & 0 & -3 \\
0 & 0 & 1 & 9
\end{array}
\right]
\;,\;
\left[
\begin{array}{ccc|c}
1 & 4  & 0 & 9 \\
0 & 0 & 1 & 7 \\
0 & 0 & 0 & 1
\end{array}
\right]
\;,\;
\left[
\begin{array}{cc}
0 & 0 \\
0 & 0 \\
0 & 0
\end{array}
\right]
$$

Again, only the first one (the identity matrix looking guy) is desirable in the context of solving a system of equations, but transforming a matrix in this form gives us an immediate and definitive answer at the question: can I solve my system?

Beyond solving a system, reshaping a matrix in this form makes it very easy to deduce other properties of the matrix, such as the rank.

For now, I hope the motivation is clear: we want to convert a matrix into Row Echelon and then Reduced Row Echelon Form to make large systems of equations trivial to solve, so we need some method to do that. What is that method called? \(Hint: It's the title of this section\)

That's right! _Gaussian Elimination_

## The Method

Here I should point out that Gaussian elimination makes sense from a purely analytical point of view. That is to say that for small systems of equations, it's relatively straightforward to do this method by hand; however, for large systems, this \(of course\) become tedious and we will need to find an appropriate numerical solution. For this reason, I have split this section into two parts. One will cover the analytical framework, and the other will cover an algorithm you can write in your favorite programming language.

In the end, reducing large systems of equations boils down to a game you play on a seemingly random matrix where you have the following moves available:

1. You can swap any two rows
2. You can multiply any row by a non-zero scale value
3. You can add any row to a multiple of any other row

That's it. Before continuing, I suggest you try to recreate the Row Echelon matrix we made above. That is, do the following:

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

After you get an upper triangular matrix, the next step is creating the Reduced Row Echelon Form. In other words, we do the following:

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

Here, the idea is similar to above. The strategy is the same as before, but starts from the right-most column and subtracts upwards instead of downwards.

## The Algorithm

Now, the analytical method may seem straightforward, but the algorithm does not obviously follow from the game we were playing before, so we'll go through it step-by-step.

Row by row, do the following process:

1. Find the highest value in the column below the pivot candidate
$$
\left[
\begin{array}{ccc|c}
\mathbf{2} & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
\mathbf{3} & -4 & 0 & 10
\end{array}
\right]
$$

If that value is $$0$$, the matrix is singular and the system has no solutions. Feel free to exit here, but I'm powering through by moving on to the next column, baby!

2. Swap the row with the highest valued element with the current row.
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
3. For all remaining rows below the pivot, find a fraction that corresponds to the ratio of the lower value in that column to the pivot \(the one you swapped to the top\)
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
A(\text{curr_row}_{\text{row}}, \text{curr_col}_{\text{col}}) \mathrel{-}=  f \times A(\text{pivot_row}_{\text{row}}, \text{pivot_row}_{\text{curr_col}}) \\
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
\mathbf{1-\frac{3}{3}} & \mathbf{2-\frac{-4}{3}} & \mathbf{3 -\frac{0}{3}} & \mathbf{4-\frac{10}{3}} \\
2 & 3  & 4 & 6
\end{array}
\right]
\rightarrow
\left[
\begin{array}{ccc|c}
3 & -4 & 0 & 10 \\
\mathbf{0} & \mathbf{\frac{10}{3}} & \mathbf{3} & \mathbf{\frac{2}{3}} \\
2 & 3  & 4 & 6
\end{array}
\right]
$$
You may set the values below the pivot to 0 for numerical reasons.


In code, this looks like:

{% method %}
{% sample lang="jl" %}
[import:1-42, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="hs" %}
[import:4-33, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% endmethod %}

As with all code, it takes time to fully absorb what is going on and why everything is happening; however, I have tried to comment the above code with the necessary steps. Let me know if anything is unclear!

Now, to be clear: this algorithm creates an upper-triangular matrix. In other words, it only creates a matrix in *Row Echelon Form*, not * **Reduced** Row Echelon Form*! So what do we do from here? Well, we continue further reducing the matrix, but with a twist: using the *Back-Substitution*.

The back-substitution method is precisely what we said above, but for every pivot starting from the bottom right one.
If we have a matrix in Row Echelon Form, we can directly solve for $$z$$, and then plug that value in to find $$y$$ and then plug both of those values in to find $$x$$!
Even though this seems straightforward, the code might not be as simple as you thought, especially if your matrix is singular.

{% method %}
{% sample lang="jl" %}
[import:44-64, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="hs" %}
[import:35-43, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% endmethod %}

Now, as for what's next... Well, we are in for a treat! The above algorithm clearly has 3 `for` loops, and will thus have a complexity of $$\sim O(n^3)$$, which is abysmal! If we can reduce the matrix to a specifically **tridiagonal** matrix, we can actually solve the system in $$\sim O(n)$$! How? Well, we can use an algorithm known as the _Tri-Diagonal Matrix Algorithm_ \(TDMA\) also known as the _Thomas Algorithm_.

### Example Code

The full code can be seen here:

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/gaussianElimination.hs)
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
