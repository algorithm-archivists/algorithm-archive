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

##### Dependencies
* [Gaussian Elimination](gaussian_elimination.md)

# Thomas Algorithm

As alluded to in the Gaussian Elimination Chapter, the Thomas Algorithm (or TDMA -- Tri-Diagonal Matrix Algorithm) allows for programmers to **massively** cut the computational cost of their code from $$\sim O(n^3) \rightarrow \sim O(n)$$! This is done by exploiting a particular case of Gaussian Elimination, particularly the case where our matrix looks like:

$$
\left[
    \begin{array}{ccccc|c}
        b_0 & c_0 & & & & d_0 \\ 
        a_1 & b_1 & c_1 & & & d_1 \\
        & a_2 & \ddots & & & \vdots \\ 
        & & & & c_{n-1}& d_{n-1} \\
        & & & a_n & b_n & d_n
    \end{array}
\right]
$$

By this, I mean that our matrix is *Tri-Diagonal* (excluding the right-hand side of our system of equations, of course!). Now, at first, it might not be obvious how this helps; however, we may divide this array into separate vectors corresponding to $$a$$, $$b$$, $$c$$, and $$d$$ and then solve for $$x$$ with back-substitution, like before. 

In particular, we need to find an optimal scale factor for each row and use that. What is the scale factor? Well, it is the diagonal $$-$$ the multiplicative sum of the off-diagonal elements. In the end, we will update $$c$$ and $$d$$ to be$$c'$$ and $$d'$$ like so:

$$
\begin{align}
c'_i &= \frac{c_i}{b_i - a_i \times c'_{i-1}} \\ 
d'_i &= \frac{d_i - a_i*d'_{i-1}}{b_i - a_i \times c'_{i-1}}
\end{align}
$$

Of course, the initial elements will need to be specifically defined as

$$
\begin{align}
c'_0 = \frac{c_0}{b_0}
d'_0 = \frac{d_0}{b_0}
\end{align}
$$

In code, this will look like this:

```julia
function(a::Vector{Float64}, b::Vector{Float64}, c::Vector{Float64},
         d::Vector{Float64}, soln::Vector{Float64})
    # Setting initial elements
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    for i = 1:n
        # Scale factor is for c and d
        scale = 1 / (b[i] - c[i-1]*a[i])
        c[i] = c[i] * scale
        d[i] = (d[i] - a[i] * d[i-1]) * scale
    end

    # Set the last solution for back-substitution
    soln[n-1] = d[n-1]

    # Back-substitution
    for i = n-2:0
        soln[i] = d[i] - c[i] * soln[i+1]
    end
    
end
```

This is a much simpler implementation than Gaussian Elimination and only has one for loop before back-substitution, which is why it has a better complexity case.
