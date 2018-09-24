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

This matrix form has a particular name: _Row Echelon Form_.
Basically, any matrix can be considered in row echelon form if

1. All non-zero rows are above rows of all zeros
2. The leading coefficient or _pivot_ (the first non-zero element in every row when reading from left to right) is right of the pivot of the row above it.

All the following examples are in the row echelon form:

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
0 & 2 \\
0 & 0
\end{array}
\right]
$$

The first two are probably the ones we are interested in, at the very least they have the right dimensions to solve a system of equations.
The last two systems are either under- or over-constrained; however, if you translate the last row of second matrix into a system, you get $$0=1$$, which is a contradiction.
This is due to the fact that the matrix is singular, and there are no solutions to this particular system. Nevertheless, all of these matrices are in row echelon form.

Now, it seems obvious to point out that if we ignore the last column, row echelon form is an upper triangular matrix.
This might not be important now, but it will play an important role in future discussions, so keep it buzzing in the back of your brain.

Now, row echelon form is nice, but wouldn't it be even better if our system of equations looked simply like this


$$
\begin{align}
x &= \frac{18}{11} \\
y &= \frac{-14}{11} \\
z &= \frac{18}{11}
\end{align}
$$


Then we would know exactly what $$x$$, $$y$$, and $$z$$ are without any fuss! In matrix form, it looks like


$$
\left[
\begin{array}{ccc|c}
1 & 0 & 0 & \frac{18}{11} \\
0 & 1 & 0 & \frac{-14}{11} \\
0 & 0 & 1 & \frac{18}{11}
\end{array}
\right]
$$

And that's where we really want to get to for obvious reasons.
This introduces yet another matrix configuration: * **Reduced** Row Echelon Form*.
A matrix is in reduced row echelon form if it satisfies the following conditions:

1. It is in row echelon form.
2. Every pivot is 1 and is the only nonzero entry in its column.

All the following examples are in the reduced row echelon form:

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

Again, only the first one (the identity matrix looking guy) is desirable in the context of solving a system of equations, but transforming any matrix in this form gives us an immediate and definitive answer at the question: can I solve my system?

Beyond solving a system, reshaping a matrix in this form makes it very easy to deduce other properties of the matrix, such as the rank.
The rank of a matrix is the maximal number of linearly independent columns, in reduced row echelon form, the rank is simply the number of pivots.

For now, I hope the motivation is clear: we want to convert a matrix into row echelon and then reduced row echelon form to make large systems of equations trivial to solve, so we need some method to do that.
In general, the term *Gaussian Elimination* refers to the process of transforming a matrix into row echelon form, and the process of transforming a row echelon matrix into reduced row echelon is called *Gauss-Jordan Elimination*.
That said, the notation here is sometimes inconsistent.
Several authors use the term *Gaussian Elimination* to include Gauss-Jordan elimination as well.
In addition, the process of Gauss-Jordan elimination is sometimes called *Back-substitution*, which is also confusing because the term can also be used to mean solving a system of equations from row echelon form, without simplifying to reduced row echelon form.
For this reason, we will be using the following definitions in this chapter:

* **Gaussian Elimination:** The process of transforming a matrix into row echelon form
* **Gauss-Jordan Elimination:** The process of transforming a row echelon matrix into *reduced* row echelon form
* **Back-substitution:** The process of directly solving a row echelon matrix, *without transforming into reduced row echelon form*

## The Method

Here I should point out that Gaussian elimination makes sense from a purely analytical point of view.
For small systems of equations, it's relatively straightforward to do this method by hand; however, for large systems, this \(of course\) become tedious and we will need to find an appropriate numerical solution.
For this reason, I have split this section into two parts. One will cover the analytical framework, and the other will cover an algorithm you can write in your favorite programming language.

In the end, reducing large systems of equations boils down to a game you play on a seemingly random matrix with 3 possible moves. You can:

1. swap any two rows
2. multiply any row by a non-zero scale value
3. add any row to a multiple of any other row

That's it.
Before continuing, I suggest you try to recreate the row echelon matrix we made above.
That is, do the following:

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

There are plenty of different strategies you could use to do this, and no one strategy is better than the rest.
Personally, I usually try to multiply each row in the matrix by different values and add rows together until the first column is all the same value, and then I subtract the first row from all subsequent rows.
I then do the same thing for the following columns.

After you get an upper triangular matrix, the next step is diagonalizing to create the reduced row echelon form. In other words, we do the following:

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
1 & 0 & 0 & \frac{18}{11} \\
0 & 1 & 0 & \frac{-14}{11} \\
0 & 0 & 1 & \frac{18}{11}
\end{array}
\right]
$$

Here, the idea is similar to above.
The strategy is the same as before, but starts from the right-most column and subtracts upwards instead of downwards.

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
If that value is $$0$$, the matrix is singular and the system has no solutions.
Feel free to exit here, but if we want to be as general as possible the algorithm can continue even in that case.

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
[import:1-45, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:15-48, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% sample lang="rs" %}
[import:41-78, lang:"rust"](code/rust/gaussian_elimination.rs)
{% sample lang="hs" %}
[import:10-36, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import:3-28, lang:"python"](code/python/gaussian_elimination.py)
{% endmethod %}

Now, to be clear: this algorithm creates an upper-triangular matrix.
In other words, it only creates a matrix in *row echelon form*, not * **reduced** row echelon form*.
If the matrix is found to be singular during this process, the system of equations is either over or under-determined and no general solution exists.
For this reason, many implementations of this method will stop the moment the matrix is found to be singular.
In this implementation, we allowed for the more general case and opted to simply output when the matrix is singular instead.
If you intend to solve a system of equations, then it makes sense to stop the method the moment you know there is no general solution, so some small modification might be necessary!

So what do we do from here? Well, we continue further reducing the matrix; however, there are two ways to do this:

1. Reduce the matrix further into *reduced* row echelon form with Gauss-Jordan elimination
2. Solve the system directly with *back-substitution* if the matrix is allows for such solutions

Let's start with Gauss-Jordan Elimination and then back-substitution

## Gauss-Jordan Elimination

Gauss-Jordan Elimination is precisely what we said above.
We basically need to find the pivot of every row and set that value to 1.
Afterwards, we subtract upwards until all values above the pivot are 0 before moving on to the next column.
Here it is in code:

{% method %}
{% sample lang="jl" %}
[import:67-93, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:64-82, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% sample lang="rs" %}
This code does not exist yet in rust, so here's Julia code (sorry for the inconvenience)
[import:67-93, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="hs" %}
[import:38-46, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import:31-49, lang:"python"](code/python/gaussian_elimination.py)
{% endmethod %}

## Back-substitution

The idea of back-substitution is straightforward: we create a matrix of solutions and iteratively solve for each variable by plugging in all variables before it.
For example, if our matrix looks like this:

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18
\end{array}
\right]
$$

We can quickly solve $$11z = 18$$ for $$z$$, and then use that to solve $$y + 2z = 2$$ for $$y$$ by plugging in for $$z$$.
After that, we simply need to solve $$2x + 3y + 4z = 6$$ for $$x$$ in a similar fashion.
In code, this involves keeping a rolling sum of all the values we substitute in like so:

{% method %}
{% sample lang="jl" %}
[import:47-64, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:50-62, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% sample lang="rs" %}
[import:79-94, lang:"rust"](code/rust/gaussian_elimination.rs)
{% sample lang="hs" %}
[import:48-53, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import:52-64, lang:"python"](code/python/gaussian_elimination.py)
{% endmethod %}

## Conclusions

And with that, we have two possible ways to reduce our system of equations.
If we are sure our matrix is not singular and that a solution exists, it's fastest to use back-substitution to find our solution.
If no solution exists or we are trying to find a reduced row echelon matrix, then Gauss-Jordan elimination is best.
As we said at the start, the notation for Gaussian Elimination is rather ambiguous in the literature, so we are hoping that the definitions provided here are clear and consistent enough to cover all the bases.

As for what's next... Well, we are in for a treat! The above algorithm clearly has 3 `for` loops, and will thus have a complexity of $$\sim O(n^3)$$, which is abysmal! If we can reduce the matrix to a specifically **tridiagonal** matrix, we can actually solve the system in $$\sim O(n)$$! How? Well, we can use an algorithm known as the _Tri-Diagonal Matrix Algorithm_ \(TDMA\) also known as the _Thomas Algorithm_.

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/gaussian_elimination.c)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/gaussian_elimination.rs)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import, lang:"python"](code/python/gaussian_elimination.py)
{% endmethod %}


<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
