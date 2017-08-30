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

* [Differential Equations](../mathematical_background/differential_equations.md)
* [Taylor Series Expansions](../mathematical_background/taylor_series.md)
* [Thomas Algorithm](../computational_mathematics/matrix_methods/thomas.md)
  * [Gaussian Elimination](../computational_mathematics/matrix_methods/gaussian_elimination.md)

# Euler Method

The Euler methods are some of the simplest methods to solve ordinary differential equations numerically. These methods introduce a new set of methods called the [Runge Kutta](runge_kutta.md) methods, which will be discussed in the near future! 

As a physicist, I tend to understand things through methods that I have learned before. In this case, it makes sense for me to see Euler methods as extensions of the [Taylor Series Expansion](../mathematical_background/taylor_series.md). These expansions basically approximate functions based on their derivatives, like so:

$$
f(x) \simeq f(a) + \frac{1}{1!}\frac{df(a)}{da}(x-a) 
    + \frac{1}{2!}\frac{d^2f(a)}{da^2}(x-a)^2 + 
    + \frac{1}{3!}\frac{d^3f(a)}{da^3}{x-a}^3 + \cdots
$$

Like before,  $$f(x)$$ is some function along real or complex space, $$a$$ is the point that we are expanding from, and $$f^{(n)}(x)$$ denotes the $$n^{\text{th}}$$ derivative of $$f(x)$$.

So, what does this mean? Well, as mentioned, we can think of this similarly to the kinematic equation:

$$
x = x_0 + vt + \frac{1}{2}at^2
$$
where $$x$$ is position, $$v$$ is velocity, and $$a$$ is acceleration.
This equation allows us to find the position of an object based on it's previous position ($$x_0$$), the derivative of it's position with respect to time ($$\frac{dx}{dt} = v_0$$) and one derivative on top of that ($$\frac{d^2x}{dx^2} = a$$). As stated in the Tayor Series Expansion, the acceleration term must also have $$\frac{1}{2!}$$ in front of it.

Now, how does this relate to the Euler methods? Well, with these methods, we assume that we are looking for a position in some space, usually denoted as $$y(t)$$, but we can use any variable. The methods assume that we have some function to evaluate the derivative of $$y(t)$$. In other words, we know that $$\frac{dy(t)}{dt} = f(t,y(t))$$. For the kinematic equation, we know what this is!

$$
\frac{dy(t)}{dt} = v = f(t,y(t)) = v_0 + a(t)
$$

So, we can iteratively solve for position by first solving for velocity. By following the kinematic equation (or Taylor Series Expansion), we find that

$$
y_{n+1} = y_n + dt f(t_n, y_n)
$$

For any timestep $$dt$$. This means that if we are solving the kinematic equation, we simply have the following equations:

$$
\begin{align}
    x_{n+1} &= x_n + v dt
    v_{n+1} &= a t_n
\end{align}
$$

Now, solving this set of equations in this way is known as the *forward* Euler Method. In fact, there is another method known as the *backward* Euler Method, which will be solved in the following section. For now, it is important to note that the stability of these methods depend on the timestep chosen. For example, in FIGURE we see dramatically different results for different timesteps. 

### Backward Euler Method

Unlike the forward Euler Method above, the backward Euler Method is an *implicit method*, which means that it results in a system of equations to solve. Luckily, we know how to solve systems of equations (*hint*: [Thomas Algorithm](../computational_mathematics/matrix_methods/thomas.md), [Gaussian Elimination](../computational_mathematics/matrix_methods/gaussian_elimination.md))
