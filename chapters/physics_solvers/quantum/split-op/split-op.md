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
As mentioned in the [quantum systems](../quantum.md) section, we can represent a a quantum wavefunction in momentum space, which is parameterized with the wavevector $$k$$.
In the hamiltonian shown above, we can split our system into real-space components, $$\hat{H}_R = \left[V(\mathbf{r}) + g|\Psi(\mathbf{r},t)|^2 \right] \Psi(\mathbf{r},t)$$, and momentum space components, $$\hat{H}_M = \left[-\frac{\hbar^2}{2m}\nabla^2 \right]\Psi(\mathbf{r},t)$$.
If we assume a somewhat general solution to our quantum system:

$$
\Psi(\mathbf{r},t + dt) = \left[e^{-\frac{i\hat{H}dt}{\hbar}}\right]\Psi(\mathbf{r},t) = \left[e^{-\frac{i(\hat{H}_R + \hat{H}_M)dt}{\hbar}}\right]\Psi(\mathbf{r},t)
$$

and assume we are simulating our system by a series of small tiemsteps ($$dt$$), we can perform similar splitting by using the Baker-Campbell-Housdorff formula:

$$
\Psi(\mathbf{r},t+dt) = \left[e^{-\frac{i\hat{H}_Rdt}{\hbar}}e^{-\frac{i\hat{H}_Mdt}{\hbar}}e^{-\frac{[i\hat{H}_R, i\hat{H}_M]dt^2}{2}}\right]\Psi(\mathbf{r},t)
$$

This accrues a small amount of error ($$dt^2$$) related to the commutation of the real and momentum-space components of the Hamiltonian. That's not okay.
In order to change the $$dt^2$$ error to $$dt^3$$, we can split the system by performing a half-step in real space before doing a full-step in momentum space, through a process called _Strang Splitting_ like so:

$$
\Psi(\mathbf{r},t+dt) = \left[e^{-\frac{i\hat{H}_Rdt}{2\hbar}}e^{-\frac{i\hat{H}_Mdt}{\hbar}}e^{-\frac{i\hat{H}_Rdt}{2\hbar}} \right]\Psi(\mathbf{r},t) + \mathcal{O}(dt^3)
$$

We can then address each part of this solution in chunks, first in real space, then in momentum space, then in real space again by using [Fourier Transforms](../../../FFT/cooley_tukey.md).
Which looks something like this:

$$
\Psi(\mathcal{r}, t+dt) = \left[\hat{U}_R(\frac{dt}{2})\mathcal{F}\left[\hat{U}_M(dt) \mathcal{F} \left[\hat{U}_R(\frac{dt}{2}) \Psi(\mathbf{r},t) \right] \right] \right] + \mathcal{O}(dt^3)
$$

where $$\hat{U}_R = e^{-\frac{i\hat{H}_Rdt}{\hbar}}$$, $$\hat{U}_M = e^{-\frac{i\hat{H}_Mdt}{\hbar}}$$, and $$\mathcal{F}$$ and $$\mathcal{F}^{-1}$$ indicate forward and inverse Fourier Transforms.

As a small concession here, using this method enforces periodic boundary conditions, where the wavefunction will simply slide from one side of your simulation box to the other, but that's fine for most cases.
In fact, for many cases (such as large-scale turbulence models) it's ideal.

Luckily, the code in this case is pretty straightforward.
Frist, we need to set all the initial parameters, including the initial grids in real and momentum space:

{% method %}
{% sample lang="jl" %}
[import:4-31, lang:"julia"](code/julia/split_op.jl)
{% endmethod %}

As a note, when we generate our grid in momentum space `k`, we need to split the grid into two lines, one that is going from `0` to `-kmax` and is then discontinuous and goes from `kmax` to `0`.
This is simply because the FFT will naturally assume that the `0` in our grid is at the left side of the simulation, so we shift k-space to match this expectation.
Afterwards, we turn them into operators:

{% method %}
{% sample lang="jl" %}
[import:32-41, lang:"julia"](code/julia/split_op.jl)
{% endmethod %}

Here, we use a standard harmonic potential for the atoms to sit in and a gaussian distribution for an initial guess for the probability distribution.
As a note, if we run this simulation in _imaginary time_, by simply setting $$\tau = it$$ and stepping through $$\tau$$, we will no longer see an "real-world" example of how the atoms should behave, but will instead see an exponential decay of higher-energy states.
This means that we can find the ground state of our system by running the simulation in imaginary time, which is an incredibly useful feature!

And finally go step-by-step through the simulation:

{% method %}
{% sample lang="jl" %}
[import:42-69, lang:"julia"](code/julia/split_op.jl)
{% endmethod %}

And that's it.
The Split-Operator method is one of the most commonly used quantum simulation algorithms because of how straightforward it is to code and how quickly you can start really digging into the physics of the simulation results!

# Example Code
{% method %}
{% sample lang="jl" %}
### Julia
[import, lang:"julia"](code/julia/split_op.jl)
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

