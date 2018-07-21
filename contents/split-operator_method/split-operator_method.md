# The Split-Operator Method
The Split-Operator Method (also called the Split-Step Method), was actually the primary method I used to solve the Schrodinger equation during my PhD.
It is one of the simplest and fastest methods for this purpose and is widely used throughout modern quantum research in the area -- in particular when dealing with the Non-linear Schrodinger Equation (NLSE):

$$
i \hbar \frac{\partial \Psi(\mathbf{r},t)}{\partial t} = \left[-\frac{\hbar^2}{2m}\nabla^2 + V(\mathbf{r}) + g|\Psi(\mathbf{r},t)|^2 \right] \Psi(\mathbf{r},t),
$$

which follows from the notation provided in the [quantum systems](../../general/quantum_systems/quantum_systems.md) chapter: $$\Psi(\mathbf{r},t)$$ is a quantum wave-function with spatial ($$\mathbf{r}$$) and time ($$t$$) dependence, $$\nabla^2$$ is a laplacian, and $$V(\mathbf{r})$$ is a potential of some sort (like $$\omega x^2$$ or something)
In this case, we also add an interaction term $$g$$ next to a nonlinear $$|\Psi(\mathbf{r},t)|^2$$ term.
This is the system I studied for most of my PhD (granted, we played a few tricks with parallelization and such, so it was _slightly_ more complicated).

At it's heart, the split-op method is nothing more than a pseudo-spectral differential equation solver... That is to say, it solves the Schrodinger equation with [FFT's](../cooley_tukey/cooley_tukey.md).
In fact, there is a large class of spectral and pseudo-spectral methods used to solve a number of different physical systems, and we'll definitely be covering those in the future.
As mentioned in the [quantum systems](../quantum_systems/quantum_systems.md) section, we can represent a quantum wavefunction in momentum space, which is parameterized with the wavevector $$k$$.
In the Hamiltonian shown above, we can split our system into position space components, $$\hat{H}_R = \left[V(\mathbf{r}) + g|\Psi(\mathbf{r},t)|^2 \right] \Psi(\mathbf{r},t)$$, and momentum space components, $$\hat{H}_M = \left[-\frac{\hbar^2}{2m}\nabla^2 \right]\Psi(\mathbf{r},t)$$.
I'll be honest, I didn't know what notation to use for $$\hat H_R$$.
After all, $$p$$ is used to describe momentum.
I settled on $$R$$ for _real space_, but that is somewhat notationally ambiguous.
Bad notation aside, let's continue.

If we assume a somewhat general solution to our quantum system:

$$
\Psi(\mathbf{r},t + dt) = \left[e^{-\frac{i\hat{H}dt}{\hbar}}\right]\Psi(\mathbf{r},t) = \left[e^{-\frac{i(\hat{H}_R + \hat{H}_M)dt}{\hbar}}\right]\Psi(\mathbf{r},t)
$$

and assume we are simulating our system by a series of small timesteps ($$dt$$), we can perform similar splitting by using the Baker-Campbell-Housdorff formula:

$$
\Psi(\mathbf{r},t+dt) = \left[e^{-\frac{i\hat{H}_Rdt}{\hbar}}e^{-\frac{i\hat{H}_Mdt}{\hbar}}e^{-\frac{[i\hat{H}_R, i\hat{H}_M]dt^2}{2}}\right]\Psi(\mathbf{r},t)
$$

This accrues a small amount of error ($$dt^2$$) related to the commutation of the real and momentum-space components of the Hamiltonian.
This is a relatively large error and that's not okay.
In order to change the $$dt^2$$ error to $$dt^3$$, we can split the system by performing a half-step in position space before doing a full-step in momentum space, through a process called _Strang Splitting_ like so:

$$
\Psi(\mathbf{r},t+dt) = \left[e^{-\frac{i\hat{H}_Rdt}{2\hbar}}e^{-\frac{i\hat{H}_Mdt}{\hbar}}e^{-\frac{i\hat{H}_Rdt}{2\hbar}} \right]\Psi(\mathbf{r},t) + \mathcal{O}(dt^3)
$$

We can then address each part of this solution in chunks, first in position space, then in momentum space, then in position space again by using [Fourier Transforms](../cooley_tukey/cooley_tukey.md).
Which looks something like this:

$$
\Psi(\mathcal{r}, t+dt) = \left[\hat{U}_R(\frac{dt}{2})\mathcal{F}^{-1}\left[\hat{U}_M(dt) \mathcal{F} \left[\hat{U}_R(\frac{dt}{2}) \Psi(\mathbf{r},t) \right] \right] \right] + \mathcal{O}(dt^3)
$$

where $$\hat{U}_R = e^{-\frac{i\hat{H}_Rdt}{\hbar}}$$, $$\hat{U}_M = e^{-\frac{i\hat{H}_Mdt}{\hbar}}$$, and $$\mathcal{F}$$ and $$\mathcal{F}^{-1}$$ indicate forward and inverse Fourier Transforms.
Here's a flowchart of what we are looking for every timestep:

<p>
    <img  class="center" src="res/split_op_method.svg" width="500" />
</p>


For the most part, that's it.
Flip to momentum space. Multiply.
Flip to position space. Multiply again.
If we guess that our initial wavefunction is gaussian-like and is slightly offset from the center or the trap, this should allow us to see our wavefunction "sloshing" back and forth in our trap, like so:

<p>
    <img  class="center" src="res/real_time.gif" width="500" />
</p>

As a small concession, using this method enforces periodic boundary conditions, where the wavefunction will simply slide from one side of your simulation box to the other, but that's fine for most cases.
In fact, for many cases (such as large-scale turbulence models) it's ideal.

That said, there is more to the story.
As we mentioned in the [quantum systems](../quantum_systems/quantum_systems.md) section, many simulations of quantum systems desire to find the ground state of our system.
The split-operator method can be used for that too!
If we run this simulation in _imaginary time_, by simply setting $$\tau = it$$ and stepping through $$\tau$$ instead of $$t$$, we will no longer see an "real-world" example of how the atoms should behave, but will instead see an exponential decay of higher-energy states.
If we run the simulation for long enough with a small enough timestep, all higher energy states will vanish.
This means that we can find the ground state of our system by running the simulation in imaginary time, which is an incredibly useful feature!
If we run the same simulation as above in imaginary time, we should see our wavefunction smoothly move to the center of our trap (the lowest energy position), like so:

<p>
    <img  class="center" src="res/imaginary_time.gif" width="500" />
</p>


## The Algorithm

Luckily, the code in this case is pretty straightforward.
As a note before starting, we will be using some incredibly theoretical units in this simulation where $$\hbar = c = 1$$.
Ironically, these units are called _natural_ units.
Many of you (*cough* experimentalists *cough*) will probably think that these units are completely unphysical, and they are; however, they allow us to output fractions and whole numbers.
For example, if we are trying to find the energy of the ground state of atoms in a simple harmonic oscillator, we know it should be $$\frac{1}{2}\hbar \omega$$, where $$\omega$$ is the coefficient in front of the $$x^2$$ term known as the _frequency_ of the trap.
If we were to calculate the energy in real units, our simulation would output $$5.272859 \times 10^{-35}$$, which is hard to interpret.
By instead using natural units, we get precisely $$\frac{1}{2}$$ and we know that those are in units of $$\hbar\omega$$.

There is a huge debate over the utility of natural units, but there is no doubt that it makes the simulation easier to understand (albeit a little misleading in the end).

Regardless, we first need to set all the initial parameters, including the initial grids in real and momentum space:

{% method %}
{% sample lang="jl" %}
[import:9-32, lang:"julia"](code/julia/split_op.jl)
{% endmethod %}

As a note, when we generate our grid in momentum space `k`, we need to split the grid into two lines, one that is going from `0` to `-kmax` and is then discontinuous and goes from `kmax` to `0`.
This is simply because the FFT will naturally assume that the `0` in our grid is at the left side of the simulation, so we shift k-space to match this expectation.
Also, for this code we will be using the notation from above: `opr.R` will be the real space operators and `opr.M` will be the momentum space operators.
There is another boolean value here called `im_time`, which is for imaginary time evolution.

Afterwards, we turn them into operators:

{% method %}
{% sample lang="jl" %}
[import:34-60, lang:"julia"](code/julia/split_op.jl)
{% endmethod %}

Here, we use a standard harmonic potential for the atoms to sit in and a gaussian distribution for an initial guess for the probability distribution.
If we give either the trap or the atoms a slight offset (so the gaussian distribution of atoms does not *quite* rest at the bottom of the $$x^2$$ potential, we can see the atoms moving back and forth in the potential as we move the simulation forward in time.
This means that we can easily see the dynamics of our quantum system!
If we run the simulation in imaginary time, we will see the gaussian distribution of atoms move towards the center of the potential, which is the location with the lowest energy.

The final step is to do the iteration, itself.

{% method %}
{% sample lang="jl" %}
[import:63-113, lang:"julia"](code/julia/split_op.jl)
{% endmethod %}

And that's it.

There is something a bit odd about the simulation in imaginary time, though.
Basically, in imaginary time, we see an exponential decay of all the higher energy states, which means we are technically losing a large amount of our wavefunction density every timestep!
To solve this issue, we _renormalize_ by enforcing that $$\int_{-\infty}^{+\infty}\Psi^\ast\Psi dx = 1$$.
As you can see from the code, this involves summing the density, multiplying that sum by `dx`, and then dividing each element in the wavefunction by the `sqrt()` of that value.

The Split-Operator method is one of the most commonly used quantum simulation algorithms because of how straightforward it is to code and how quickly you can start really digging into the physics of the simulation results!

## Example Code
This example code is a simulation of a gaussian distribution of atoms slightly offset in a harmonic trap in imaginary time.
So long as the code is written appropriately, this means that the atoms should move towards the center of the trap and the energy should decay to $$\frac{1}{2}\hbar\omega$$, which will be simply $$\frac{1}{2}$$ in this simulation.
Checking to make sure your code can output the correct energy for a harmonic trap is a good test to make sure it is all working under-the-hood before simulating systems with more complicated Hamiltonians.

{% method %}
{% sample lang="jl" %}
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

