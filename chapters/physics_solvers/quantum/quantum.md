# Quantum Systems

As I am sure you have heard, the quantum world is weird.
As you deal with progressively smaller and smaller systems, at some point, it becomes less accurate to describe objects as particles.
Instead, it is better to describe objects as probability waves.
Again, this is pretty common knowledge; however, there is a distinct lack of readable literature on how to simulate quantum systems, even though there are numerous methods for exactly that!
This section will deal with the computation of quantum states with classical machines.
Now, I know what you are thinking, "Wait. Why are we simulating quantum systems on classical computers? Why not simulate it with some sort of experiment or with quantum computers?"
Well, here's where the notation get's really sticky.

There is a clear difference between quantum computers and quantum simulators.
A _quantum computer_ is the quantum analog to a classical computer, replacing bits with qubits by using quantum information theory.
Quantum computers are usually thought of as a way to use quantum mechanics to eventually solve real-world problems with new quantum algorithms.
Both Grover's and Shor's algorithms are good examples of cases where quantum computation could greatly change the landscape of modern computation as we know it!

_Quantum Simulators_ on the other hand are quantum systems used to better un
derstand quantum mechanics
Because supercomputers are not great at performing quantum computations, quantum simulators exist as a building block for quantum computation; however, their purpose is not explicity for quantum information theory.
Often times a _universal quantum simulator_ is often called a quantum computer.

The truth is that until we have real quantum simulators, simulating quantum systems on classical hardware is as good as we can do.
This section is devoted to all the different methods currently used to solve complex quantum systems, so let's start with the Schrodinger Equation, which has many different fomulations.
Here is the easiest one to explain:

$$
i \hbar \frac{\partial \Psi(\mathbf{r},t)}{\partial t} = \left[-\frac{\hbar^2}{2m} \nabla^2 + V(\mathbf{r},t) \right] \Psi(\mathbf{r},t)
$$

Where $$\Psi(\mathbf{r},t)$$ is your quantum _wavefunction_, $$V(\mathbf{r},t)$$ is a _trapping potential_, $$\nabla^2$$ is a _laplacian_, $$\mathbf{r}$$ is some sort of spatial component, and $$t$$ is time.
There is a lot to take in here; however, it's ultimately just some time derivative on the left-hand side and a spatial derivative (with some extra steps) on the right-hand side.
In this way, it isn't too different from the diffusion (heat) equation:

$$
\frac{\partial\phi(\mathbf{r},t)}{\partial t} = D \nabla^2 \phi(\mathbf{r},t)
$$

where $$D$$ is some positive definite matrix and $$\phi(\mathbf{r},t)$$ is the density (or temperature) of the system.
In fact, this is why one of the most common types of quantum simulation is via _diffusion monte carlo_.
There really isn't that much of a difference between the two systems in terms of classical simulation.

As a note: quantum mechanics works fundamentally differently than classical mechanics in physics.
The wavefunction is essentially a set of all possible states for an object to be in, where there is some probability for the particle to be found in each state.
This means that it is not possible to say that a particle is at a particular location, and instead we often say that it could be at any location with probability, as shown in the _probability density_:

$$
P(\mathbf{r}, t) = |\Psi(\mathbf{r},t)|^2 = \Psi(\mathbf{r},t)^{*}\Psi(\mathbf{r},t)
$$

Here, there are 2 things to note:

1. The absolute value squared of a complex parameter $$\Psi(\mathbf{r},t)$$ is a dot product (inner product) between a complext function and it's Hermitian conjugate
2. As you have probably heard, once a wavefunction is observed it collapses onto a single state

Now, to be clear: the probabilities must all sum to 1, or (more formally):

$$
\int_{-\infty}^{+\infty}|\Psi(\mathbf{r},t)|^2 d\mathbf{r} = 1
$$

As another note: Just like position space can be parameterized by a position vector $$\textbf{x}$$, wavefunctions can be parameterized by a _wave_vector $$\textbf{k}$$ in frequency space.
Often times, the wavevector space is called _momentum_ space, which makes sense when considering the de Broglie formula:

$$
p = \frac{h}{\lambda}
$$

where $$h$$ is Planck's constant and $$\lambda$$ is the wavelength.
This means that we can ultimately move between real and momentum space by using [Fourier Transforms](../../FFT/cooley_tukey.md), which is incredibly useful in a number of cases!

Unfortunately, the interpretation of quantum simulation is rather tricky and is ultimately easier to understand with slightly different notation.
This notation is called _braket_ notation, where a _ket_ looks like this:

$$
\lvert A \rangle
$$

And basically describes $$A$$ as a column vector.
The _bra_ represents the Hermitian conjucate of the ket and looks like this:

$$
\langle B \rvert
$$

It is often represented as a row vector for $$B$$. Because of this, $$ \langle B \rvert A \rangle $$ represents the inner product of the two vectors and $$ \lvert A \rangle \langle B \rvert $$ represents the outer product.
Now, to this point, the braket notation does not have any particularly quantum-like features; however, it becomes useful when describing actual quantum phenomenon.
For example, if we want to indicate the probability of a wavefunction $$\psi$$ collapsing onto state $$\phi$$, we might write: $$\langle \phi \rvert \psi \rangle$$, which is precisely the same as the probability density defined above.

As we proceed to add new algorithms to simulate quantum systems, I will add more and more notation to this section; however, there are already huge textbooks out there related to understanding and studying quantum systems.
We don't want to re-invent the wheel here.
Instead, we want to focus on an area that is often not considered with too much detail -- the algorithms and methods researchers use to ascertain new knowedge about quantum mechanics, like the split-operator method, DMRG, quantum monte carlo, exact diagonalization, and many more.

Quantum mechanics is one of those areas of physics that really does push the boundary of human knowledge in a number of different areas and computing is no different.
In fact, [quantum information theory](../../QI/QI.md) is currently set to be the next innovation to radically change the landscape of modern computation as we know it!
Of course, because of the large-scale effects that this will likely have on the industry, it deserved it's own section.

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

