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

# Gift Wrapping
If given a "gift", here defined as a random distribution of points in two or three dimensions, gift-wrapping algorithms allow programmers to find its convex hull -- the smallest convex shape that holds all interior points.
This is one of the many cases where the leap from two to three dimensions leads to an incredibly more complicated code.
That said, there is an rich history of algorithms to solve this problem.

To be fair, only the Jarvis March is classified as *the* gift wrapping algorithm; however, it's a neat name to give algorithms that solve for the convex hull of a distribution of points.
Strictly speaking, though, the term is not entirely accurate for all convex hull methods.
