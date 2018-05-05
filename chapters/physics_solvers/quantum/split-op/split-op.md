# The Split-Operator Method
The Split-Operator Method (also called the Split-Step Method), was actually the primary method I used to solve the Schrodinger equation during my PhD.
It is one of the simplest and fastest methods for this purpose and is widely used throughout modern quantum research in the area -- in particular when dealing with the Non-linear Schrodinger Equation:

$$
i \hbar \frac{\partial \Psi(\mathbf{r},t)}{\partial t} = \left[-\frac{\hbar^2}{2m}\nabla^2 + g|\Psi(\mathbf{r},t)|^2 \right] \Psi(\mathbf{r},t),
$$

which follows from the notation provided in the [quantum systems](../quantum.md) chapter: $$\Psi(\mathbf{r},t)$$ is a quantum wave-function with spatial ($$\mathbf{r}$$) and time ($$t$$) dependence and $$\nabla^2$$ is a laplacian; however, in this case, we also add an interaction tern $$g$$ next to a nonlinear $$|\Psi(\mathbf{r},t)|^2$$ term.
By adding in the $$V(\mathbf{r})$$ term, we get an equation used to study superfluid Bose--Einstein Condensate (BEC) systems:

$$
i \hbar \frac{\partial \Psi(\mathbf{r},t)}{\partial t} = \left[-\frac{\hbar^2}{2m}\nabla^2 + V(\mathbf{r}) + g|\Psi(\mathbf{r},t)|^2 \right] \Psi(\mathbf{r},t).
$$

This is the system I studied for most of my PhD (granted, we played a few tricks with parallelization and such, so it was _slightly_ more complicated).

At it's heart, the split-op method is nothing more than a pseudo-spectral differential equation solver... That is to say, it solves the Schrodinger equation with [FFT's](../../../FFT/cooley_tukey.md).
In fact, there is a large class of spectral and pseudo-spectral methods used to solve a number of different physical systems, and we'll definitely be covering those in the future.

For now, let's answer the obvious question, "How on Earth can FFT's be used to solve quantum systems?"
Well, looking at the equation above and using a bit of analyical differential equation knowledge, we can assume the solution will be divided into a number of basis functions like :

$$
\Psi(\mathbf{r},t) = \frac{1}{\sqrt{2 \pi}}\sum_n c_n(t) e^{2 \pi i n \mathbf{r}}
$$

where $$c_n$$ are some constants depending on "how much" of that basis function exists in the solved system.

As a small concession here, using this method enforces periodic boundary conditions, where the wavefunction will simply slide from one side of your simulation box to the other, but that's fine for most cases.
In fact, for many cases (such as large-scale turbulence models) it's ideal.

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

