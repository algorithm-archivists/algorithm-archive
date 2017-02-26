# Gaussian Elimination

So, how exactly do we go about solving a system of linear equations? Well, one way is *Gaussian Elimination*, which you may have encountered before in a math class or two. The basic idea is that we take a system of equations,

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
\right]$$

Now. At first, this doesn't seem to help anything, so let's think of this in another way. Wouldn't it be great if the system of equations looked like this:

$$
\begin{align}
2x + 3y + 4z &= 6 \\
y + 2z &= 2 \\
11z &= 18 
\end{align}
$$

Then we could just solve for $$z$$ and plug it in to the top two equations to solve for $$x$$ and $$y$$! In matrix form, it would look like this

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18 
\end{array}
\right]$$

and it has a particular name: *Row Eschelon Form*. Basically, any matrix can be considered in row eschelon form if

1. All non-zero rows are above rows of all zeros
2. The leading coefficient or *pivot* -- the first non-zero element in every row when reading from left to right is right of the pivot of the row above it.

Now, Row Eschelon form is nice, but wouldn't it be even better if our system of equations looked simply like this

$$
\begin{align}
11x &= 156 \\
11y &= 58 \\
11z &= 18 
\end{align}
$$

Then we would know exactly what $$x$$, $$y$$, and $$z$$ are without any fuss! In matrix form, it looks like

$$
\left[
\begin{array}{ccc|c}
11 & 0 & 0 & 156 \\
0 & 11 & 0 & 116 \\
0 & 0 & 11 & 18 
\end{array}
\right]$$

And again has a special name ***Reduced** Row Eschelon Form*. Now, it seems obvious to point out that if we remove the values after the equals sign ($$=$$), Row Eschelon Form is an upper triangular matrix, Reduced Row Eschelon Form is diagonal. This might not be important now, but it will play an important role in future discussions, so keep it bussing in the back of your brain. 

For now, I hope the motivation is clear: we want to convert a matrix into Row Eschelon and Reduced Row Eschelon form to make large systems of equations trivial to solve, so we need some method to do that. What is that method called? (Hint: It's the title of this section)

That's right! *Gaussian Elimination*

## The Method

Here I should point out that Gaussian elimination makes sense from a purely analytical point of view. That is to say that for small systems of equations, it's relatively straightforward to do this method by hand; however, for large systems, this (of course) become tedious and we will find an appropriate numerical solution. For this reason, I have split this section into two parts. One covers the analytical framework, and the other an algorithm you can write in your favorite programming language.

In the end, reducing large systems of equations boils down to a game you play on a seemingly random matrix where you have the following moves available:

1. You can swap any two rows
2. You can multiply any row by a non-zero scale value
3. You can add any row to a multiple of any other row.

That's it. Before continuing, I suggest you try to recreate the Row Eschelon matrix we made above. That is, do the following:

$$\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
3 & -4 & 0 & 10 
\end{array}
\right]$$ $$\rightarrow$$ $$\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18 
\end{array}
\right]$$

There are plenty of different strategies you could use to do this, and no one is better than the rest. Personally, I usually try to multiply each row in the matrix by different values and add rows together until the first column is all the same value, and then I subtract the first row from all subsequent rows. I then do the same thing for the following columns. 

After you get an upper triangular matrix, the next step is diagonalizing to create the Reduced Row Eschelon form. In other words, we do the following: 

$$\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
0 & 1 & 2 & 2 \\
0 & 0 & 11 & 18 
\end{array}
\right]$$ $$\rightarrow$$ $$\left[
\begin{array}{ccc|c}
11 & 0 & 0 & 156 \\
0 & 11 & 0 & 116 \\
0 & 0 & 11 & 18 
\end{array}
\right]$$

Here, the idea is similar to above. You can do basically anything you want. My strategy is usually the same as before, but starts from the right-most column and subtracts upwards instead of downwards.

## The Algorithm

Now, the analytical method may seem straightforward, but the algorithm does not obviously follow from the game we were playing before, so we'll go through it step-by-step.

In general, we go through the following process:

1. For each column `col`, find the highest value

$$
\left[
\begin{array}{ccc|c}
2 & 3  & 4 & 6 \\
1 & 2 & 3 & 4 \\
\mathbf{3} & -4 & 0 & 10 
\end{array}
\right]$$

2. Swap the row with the highest valued element with the `col`th row.
3. For all remaining rows, find a fraction that corresponds to the ratio of the lower value in that column to the central pivot (the one you swapped to the top)
4. Set all values in the corresponding rows to be the value they were before - the top row * the fraction. This is essentially performing move 3 from above, except with an optimal multiplicative factor.
5. Set the value of that row's pivot column to 0.

ADD VISUALIZATION OF ABOVE

In code, this looks like:

```python
# Matrix of size rows, cols
Matrix A[rows,cols]

# Main loop going through all columns
for k = 1:min(rows,cols):

    # Step 1: finding the maximum element for each column
    max_index = max(abs(A[:,k])
    
    # Check to make sure matrix is good!
    if A[max_index,k] == 0:
          matrix is singular! End!  
    end
    
    # Step 2: swap row with highest value for that column to the top
    swap(A[k,:],A[max_index,:])
    
    # Loop for all remaining rows
    for i = k+1:rows
        
        # Step 3: finding fraction
        fraction = A[i,k]/A[k,k]
        
        # loop through all columns for that row
        for j = k+1:cols
        
             # Step 4: re-evaluate each element
            A[i,j] = A[i,k] - A[k,j]*fraction
            
            # Step 5: Set lower elements to 0
            A[i,k] = 0
        end
    end
end
```

As with all code, it takes time to fully absorb what is going on and why everything is happening; however, I have tried to comment the above psuedocode with the necessary steps. Let me know if anything is unclear!

Now, as for what's next... Well, we are in for a treat! The above algorithms clearly has 3 `for` loops, and will thus have a complexity of $$\sim O(n^3)$$, which is abysmal! If we can reduce the matrix to a specifically **tridiagonal** matrix, we can actually solve the system in $$\sim O(n)$$! How? Well, we can use an algorithm known as teh *Tri-Diagonal Matrix Algorithm* (TDMA) also known as the *Thomas Algorithm*.