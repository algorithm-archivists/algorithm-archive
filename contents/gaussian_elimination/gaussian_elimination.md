# Gaussian Elimination

Let's say we have a system of equations,

$$
\begin{align}
2x + 3y + 4z &= 6 \\
x + 2y + 3z &= 4 \\
3x - 4y &= 10
\end{align}
$$

and we want to solve for $$x$$, $$y$$, and $$z$$.
Well, one way to do this is with _Gaussian Elimination_, which you may have encountered before in a math class or two.

The first step is to transform the system of equations into a matrix by using the coefficients in front of each variable, where each row corresponds to another equation and each column corresponds to an independent variable like $$x$$, $$y$$, or $$z$$.
For the previous system of equations, this might look like this:

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

At first, translating the set of equations into a matrix like this doesn't seem to help with anything, so let's think of this in another way.

#### Row Echelon Form
Instead of the complicated mess of equations shown above, imagine if the system looked like this:

$$
\begin{align}
2x + 3y + 4z &= 6 \\
y + 2z &= 2 \\
11z &= 18
\end{align}
$$

Then we could just solve for $$z$$ and plug that value in to the top two equations to solve for $$x$$ and $$y$$ through a process known as back-substitution.
In matrix form, this set of equations would look like this:

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
Basically, any matrix can be considered in row echelon form if the leading coefficient or _pivot_ (the first non-zero element in every row when reading from left to right) is right of the pivot of the row above it.

This creates a matrix that sometimes resembles an upper-triangular matrix; however, that doesn't mean that all row-echelon matrices are upper-triangular.
For example, all of the following matrices are in row echelon form:

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
1 &  2 \\
2 &  0 \\
0 &  0
\end{array}
\right]
$$

The first two of these have the right dimensions to find a solution to a system of equations; however, the last two matrices are respectively under- and over-constrained, meaning they do not provide an appropriate solution to a system of equations.
That said, this doesn't mean that every matrix in the correct form can be solved either.
For example, if you translate the second matrix into a system of equations again, the last row translates into $$0x+0y+0z=1$$, which is a contradiction.
This is due to the fact that the matrix is singular, and there are no solutions to this particular system.
Nevertheless, all of these matrices are in row echelon form.

#### *Reduced* Row Echelon Form
Row echelon form is nice, but wouldn't it be even better if our system of equations looked simply like this:

$$
\begin{align}
x &= \frac{18}{11} \\
y &= \frac{-14}{11} \\
z &= \frac{18}{11}
\end{align}
$$

Then we would know exactly what $$x$$, $$y$$, and $$z$$ are without any fuss! In matrix form, it looks like this:

$$
\left[
\begin{array}{ccc|c}
1 & 0 & 0 & \frac{18}{11} \\
0 & 1 & 0 & \frac{-14}{11} \\
0 & 0 & 1 & \frac{18}{11}
\end{array}
\right]
$$

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

Again, only the first of these (the one that looks like an identity matrix) is desirable in the context of solving a system of equations, but transforming any matrix in this form gives us an immediate and definitive answer at the question: can I solve my system of equations?

Beyond solving a system of equations, reshaping a matrix in this form makes it very easy to deduce other properties of the matrix, such as its rank &mdash; the maximum number of linearly independent columns.
In reduced row echelon form, the rank is simply the number of pivots.

For now, I hope the motivation is clear: we want to convert a matrix into row echelon and then reduced row echelon form to make large systems of equations trivial to solve, so we need some method to do that.
In general, the term *Gaussian Elimination* refers to the process of transforming a matrix into row echelon form, and the process of transforming a row echelon matrix into reduced row echelon form is called *Gauss-Jordan Elimination*.
That said, the notation here is sometimes inconsistent.
Several authors use the term *Gaussian Elimination* to include Gauss-Jordan elimination as well.
In addition, the process of Gauss-Jordan elimination is sometimes called *Back-substitution*, which is also confusing because the term can also be used to mean solving a system of equations from row echelon form, without simplifying to reduced row echelon form.
For this reason, we will be using the following definitions in this chapter:

* **Gaussian Elimination:** The process of transforming a matrix into row echelon form
* **Gauss-Jordan Elimination:** The process of transforming a row echelon matrix into *reduced* row echelon form
* **Back-substitution:** The process of directly solving a row echelon matrix, *without transforming into reduced row echelon form*

## The Analytical Method

Gaussian elimination is inherently analytical and can be done by hand for small systems of equations; however, for large systems, this \(of course\) become tedious and we will need to find an appropriate numerical solution.
For this reason, I have split this section into two parts. One will cover the analytical framework, and the other will cover an algorithm you can write in your favorite programming language.

In the end, reducing large systems of equations boils down to a game you play on a seemingly random matrix with 3 possible moves. You can:

1. Swap any two rows.
2. Multiply any row by a non-zero scale value.
3. Add any row to a multiple of any other row.

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
One method is to subtract a multiple of the top row from subsequent rows below it such that all values beneath the pivot value are zero.
This process might be easier if you swap some rows around first and can be performed for each pivot.

After you get a row echelon matrix, the next step is to find the reduced row echelon form. In other words, we do the following:

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

Here, the idea is similar to above and the same rules apply.
In this case, we might start from the right-most column and subtracts upwards instead of downwards.

## The Computational Method

The analytical method for Gaussian Elimination may seem straightforward, but the computational method does not obviously follow from the "game" we were playing before.
Ultimately, the computational method boils down to two separate steps and has a complexity of $$\mathcal{O}(n^3)$$.

As a note, this process iterates through all the rows in the provided matrix.
When we say "current row" (`curr_row`), we mean the specific row iteration number we are on at that time, and as before, the "pivot" corresponds to the first non-zero element in that row.

#### Step 1
For each element in the pivot column under the current row, find the highest value and switch the row with the highest value with the current row.
The *pivot* is then considered to be the first element in the highest swapped row.

For example, in this case the highest value is $$3$$:

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
\mathbf{3} & -4 & 0 & 10
\end{array}
\right]
$$

After finding this value, we simply switch the row with the $$3$$ to the current row:

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

In this case, the new pivot is $$3$$.

In code, this process might look like this:

{% method %}
{% sample lang="jl" %}
[import:12-24, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="java" %}
[import:14-30, lang:"java"](code/java/GaussianElimination.java)
{% sample lang="c" %}
[import:5-13, lang:"c"](code/c/gaussian_elimination.c)
[import:19-34, lang:"c"](code/c/gaussian_elimination.c)
{% sample lang="cpp" %}
[import:13-23, lang:"cpp"](code/c++/gaussian_elimination.cpp)
{% sample lang="hs" %}
[import:10-17, lang:"haskell"](code/haskell/gaussianElimination.hs)
[import:44-46, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="js" %}
[import:7-23, lang:"javascript"](code/javascript/gaussian_elimination.js)
{% sample lang="py" %}
[import:13-19, lang:"python"](code/python/gaussian_elimination.py)
{% sample lang="rs" %}
[import:43-60, lang:"rust"](code/rust/gaussian_elimination.rs)
{% endmethod %}

As a note, if the highest value is $$0$$, the matrix is singular and the system has no single solution.
This makes sense because if the highest value in a column is 0, the entire column must be 0, thus there can be no unique solution when we read the matrix as a set of equations.
That said, Gaussian elimination is more general and allows us to continue, even if the matrix is not necessarily solvable as a set of equations.
Feel free to exit after finding a $$0$$ if your end-goal is to solve a system of equations.


#### Step 2
For the row beneath the current pivot row and within the pivot column, find a fraction that corresponds to the ratio of the value in that column to the pivot, itself.
After this, subtract the current pivot row multiplied by the fraction from each corresponding row element.
This process essentially subtracts an optimal multiple of the current row from each row underneath (similar to Step 3 from the above game).
Ideally, this should always create a 0 under the current row's pivot value.

For example, in this matrix, the next row is $$1$$ and the pivot value is $$3$$, so the fraction is $$\frac{1}{3}$$.
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
    f &= A(\text{curr_row}, \text{pivot}_{\text{col}}) /  A(\text{pivot}_{\text{row}}, \text{pivot}_{\text{col}}) \\
      &= \frac{1}{3}
\end{align}
$$

After finding the fraction, we simply subtract $$\text{current_row} - \frac{1}{3}\times \text{pivot_row}$$, like so:

$$
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
0 & \mathbf{\frac{10}{3}} & \mathbf{3} & \mathbf{\frac{2}{3}}
\\
2 & 3  & 4 & 6
\end{array}
\right]
$$

After this, repeat the process for all other rows.

Here is what it might look like in code:
{% method %}
{% sample lang="jl" %}
[import:26-38, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="java" %}
[import:32-40, lang:"java"](code/java/GaussianElimination.java)
{% sample lang="c" %}
[import:36-41, lang:"c"](code/c/gaussian_elimination.c)
{% sample lang="cpp" %}
[import:25-32, lang:"cpp"](code/c++/gaussian_elimination.cpp)
{% sample lang="hs" %}
[import:19-33, lang:"haskell"](code/haskell/gaussianElimination.hs)
[import:42-42, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="js" %}
[import:25-30, lang:"javascript"](code/javascript/gaussian_elimination.js)
{% sample lang="py" %}
[import:21-26, lang:"python"](code/python/gaussian_elimination.py)
{% sample lang="rs" %}
[import:62-71, lang:"rust"](code/rust/gaussian_elimination.rs)
{% endmethod %}

#### All together
When we put everything together, it looks like this:

{% method %}
{% sample lang="jl" %}
[import:1-45, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:15-48, lang:"c"](code/c/gaussian_elimination.c)
{% sample lang="cpp" %}
[import:8-34, lang:"cpp"](code/c++/gaussian_elimination.cpp)
{% sample lang="rs" %}
[import:41-78, lang:"rust"](code/rust/gaussian_elimination.rs)
{% sample lang="hs" %}
[import:10-36, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import:3-28, lang:"python"](code/python/gaussian_elimination.py)
{% sample lang="java" %}
[import:5-47, lang:"java"](code/java/GaussianElimination.java)
{% sample lang="js" %}
[import:1-38, lang:"javascript"](code/javascript/gaussian_elimination.js)
{% endmethod %}

To be clear: if the matrix is found to be singular during this process, the system of equations is either over- or under-determined and no general solution exists.
For this reason, many implementations of this method will stop the moment the matrix is found to have no unique solutions.
In this implementation, we allowed for the more general case and opted to simply output when the matrix is singular instead.
If you intend to solve a system of equations, then it makes sense to stop the method the moment you know there is no unique solution, so some small modification of this code might be necessary!

So what do we do from here?
Well, we continue reducing the matrix; however, there are two ways to do this:

1. Reduce the matrix further into *reduced* row echelon form with Gauss-Jordan elimination
2. Solve the system directly with *back-substitution* if the matrix allows for such solutions

Let's start with Gauss-Jordan Elimination and then back-substitution

## Gauss-Jordan Elimination

Gauss-Jordan Elimination is precisely what we said above; however, in this case, we often work from the bottom-up instead of the top-down.
We basically need to find the pivot of every row and set that value to 1 by dividing the entire row by the pivot value.
Afterwards, we subtract upwards until all values above the pivot are 0 before moving on to the next column from right to left (instead of left to right, like before).
Here it is in code:

{% method %}
{% sample lang="jl" %}
[import:67-93, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:64-82, lang:"c"](code/c/gaussian_elimination.c)
{% sample lang="cpp" %}
[import:36-54, lang:"cpp"](code/c++/gaussian_elimination.cpp)
{% sample lang="rs" %}
This code does not exist yet in rust, so here's Julia code (sorry for the inconvenience)
[import:67-93, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="hs" %}
[import:38-46, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import:31-49, lang:"python"](code/python/gaussian_elimination.py)
{% sample lang="java" %}
[import:49-70, lang:"java"](code/java/GaussianElimination.java)
{% sample lang="js" %}
[import:57-76, lang:"javascript"](code/javascript/gaussian_elimination.js)
{% endmethod %}

As a note: Gauss-Jordan elimination can also be used to find the inverse of a matrix by following the same procedure to generate a reduced row echelon matrix, but with an identity matrix on the other side instead of the right-hand side of each equation.
This process is straightforward but will not be covered here, simply because there are much faster numerical methods to find an inverse matrix; however, if you would like to see this, let me know and I can add it in for completeness.

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
In code, this involves keeping a rolling sum of all the values we substitute, subtracting that sum from the solution column and then dividing by the coefficient variable.
In code, it looks like this:

{% method %}
{% sample lang="jl" %}
[import:47-64, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import:50-62, lang:"c"](code/c/gaussian_elimination.c)
{% sample lang="cpp" %}
[import:56-72, lang:"cpp"](code/c++/gaussian_elimination.cpp)
{% sample lang="rs" %}
[import:79-94, lang:"rust"](code/rust/gaussian_elimination.rs)
{% sample lang="hs" %}
[import:48-53, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import:52-64, lang:"python"](code/python/gaussian_elimination.py)
{% sample lang="java" %}
[import:72-87, lang:"java"](code/java/GaussianElimination.java)
{% sample lang="js" %}
[import:40-55, lang:"javascript"](code/javascript/gaussian_elimination.js)
{% endmethod %}

## Visual Representation

We have thus far used Gaussian elimination as a method to solve a system of equations; however, there is often a much easier way to find a similar solution simply by plotting each row in our matrix.
For the case of 2 equations and 2 unknowns, we would plot the two lines corresponding to each equation and the $$(x, y)$$ location of their point of intersection would be the solution for $$x$$ and $$y$$.
Similarly, for the case of 3 equations and 3 unknowns, we would plot 3 planes and the $$(x, y, z)$$ location of their point of intersection would be the solution for $$x$$, $$y$$, and $$z$$.

What, then, is the point of Gaussian elimination if we can simply plot our set of equations to find a solution?
Well, this analogy breaks down quickly when we start moving beyond 3D, so it is obvious we need some method to deal with higher-dimensional systems.
That said, it is particularly interesting to see what happens as we plot our matrix during Gaussian elimination for the 3D case.

<div style="text-align:center">
<video width="560" height="315" autoplay controls loop>
  <source src="res/GE_vis.mp4" type="video/mp4">
Your browser does not support the video tag.
</video> 
</div>

As we can see in the above visualization, the planes wobble about in 3D until they reach row echelon form, where one plane is parallel to the $$x$$ and $$y$$ axes.
At this point, it's trivial to find the $$z$$-coordinate for the solution because it's simply the $$z$$ intercept of the parallel plane.
From there, the matrices become even easier to interpret as they move to the reduced row echelon form.
In this form, the solution is simply the $$x$$, $$y$$, and $$z$$ intercepts of the appropriate planes.

This visualization might have been obvious for some readers, but I found it particularly enlightening at first.
By performing Gaussian elimination, we are manipulating our planes such that they can be interpreted at a glance -- which is precisely the same thing we are doing with the matrix interpretation!

## Conclusions

And with that, we have two possible ways to reduce our system of equations and find a solution.
If we are sure our matrix is not singular and that a solution exists, it's fastest to use back-substitution to find our solution.
If no solution exists or we are trying to find a reduced row echelon matrix, then Gauss-Jordan elimination is best.
As we said at the start, the notation for Gaussian Elimination is rather ambiguous in the literature, so we are hoping that the definitions provided here are clear and consistent enough to cover all the bases.

As for what's next... Well, we are in for a treat!
The above algorithm clearly has 3 `for` loops and has a complexity of $$\sim O(n^3)$$, which is abysmal!
If we can reduce the matrix to a specifically **tridiagonal** matrix, we can actually solve the system in $$\sim O(n)$$!
How? Well, we can use an algorithm known as the _Tri-Diagonal Matrix Algorithm_ \(TDMA\) also known as the [_Thomas Algorithm_](../thomas_algorithm/thomas_algorithm.md).

There are also plenty of other solvers that do similar things that we will get to in due time.

## Video Explanation

Here's a video describing Gaussian elimination:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/2tlwSqblrvU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/gaussian_elimination.jl)
{% sample lang="c" %}
[import, lang:"c"](code/c/gaussian_elimination.c)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/gaussian_elimination.cpp)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/gaussian_elimination.rs)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/gaussianElimination.hs)
{% sample lang="py" %}
[import, lang:"python"](code/python/gaussian_elimination.py)
{% sample lang="java" %}
[import, lang:"java"](code/java/GaussianElimination.java)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/gaussian_elimination.js)
{% endmethod %}


<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Images/Graphics
- The animation "[GEvis](res/GE_vis.mp4)" was created by [James Schloss](https://github.com/leios) and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
