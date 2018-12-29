# The Forward Euler Method

The Euler methods are some of the simplest methods to solve ordinary differential equations numerically.
They introduce a new set of methods called the Runge Kutta methods, which will be discussed in the near future!

As a physicist, I tend to understand things through methods that I have learned before.
In this case, it makes sense for me to see Euler methods as extensions of the [Taylor Series Expansion](../taylor_series_expansion/taylor_series_expansion.md).
These expansions basically approximate functions based on their derivatives, like so:

$$
f(x) \approx f(a) + \frac{1}{1!}\frac{df(a)}{da}(x-a)
    + \frac{1}{2!}\frac{d^2f(a)}{da^2}(x-a)^2
    + \frac{1}{3!}\frac{d^3f(a)}{da^3}(x-a)^3 + \cdots
$$

Like before,  $$f(x)$$ is some function along real or complex space, $$a$$ is the point that we are expanding from, and $$f^{(n)}(x)$$ denotes the $$n^{\text{th}}$$ derivative of $$f(x)$$.

So, what does this mean? Well, as mentioned, we can think of this similarly to the kinematic equation:
$$
x = x_0 + vt + \frac{1}{2}at^2
$$
where $$x$$ is position, $$v$$ is velocity, and $$a$$ is acceleration.
This equation allows us to find the position of an object based on it's previous position ($$x_0$$), the derivative of it's position with respect to time ($$\frac{dx}{dt} = v_0$$) and one derivative on top of that ($$\frac{d^2x}{dx^2} = a$$).
As stated in the Tayor Series Expansion, the acceleration term must also have $$\frac{1}{2!}$$ in front of it.

Now, how does this relate to the Euler methods?
Well, with these methods, we assume that we are looking for a position in some space, usually denoted as $$y(t)$$, but we can use any variable.
The methods assume that we have some function to evaluate the derivative of $$y(t)$$. In other words, we know that $$\frac{dy(t)}{dt} = f(t,y(t))$$.
For the kinematic equation, we know what this is!

$$
\frac{dy(t)}{dt} = v = f(t,y(t)) = v_0 + a(t)
$$

So, we can iteratively solve for position by first solving for velocity. By following the kinematic equation (or Taylor Series Expansion), we find that

$$
y_{n+1} = y_n + f(t_n, y_n) dt
$$

For any timestep $$dt$$. This means that if we are solving the kinematic equation, we simply have the following equations:

$$
\begin{align}
    x_{n+1} &= x_n + v dt \\
    v_{n+1} &= a (t_n)
\end{align}
$$

Now, solving this set of equations in this way is known as the *forward* Euler Method.
In fact, there is another method known as the *backward* Euler Method, which we will get to soon enough.
For now, it is important to note that the error of these methods depend on the timestep chosen.

<p>
    <img  class="center" src="res/error.png" />
</p>

For example, here we see dramatically different results for different timesteps for solving the ODE $$y' = \frac{x^3}{6}$$, whose solution is $$y = \frac{x^2}{2}$$.
The blue line is the analytical solution, the green is with a timestep of 0.5 and the red is with a timestep of 1.
To be clear: the larger the timestep, the worse the error becomes; however, there is at least one more problem with using the forward Euler method on real problems: instabilities.

As we mentioned, the forward Euler method approximates the solution to an Ordinary Differential Equation (ODE) by using only the first derivative.
This is (rather expectedly) a poor approximation.
In fact, the approximation is so poor that the error associated with running this algorithm can add up and result in incredibly incorrect results.
As you might imagine, the only solution to this is decreasing the timestep and hoping for the best or using a similar method with different stability regions, like the backward Euler method.

Let's assume we are solving a simple ODE: $$y' = -3y, y(0) = 1$$.
The solution here is $$y(t) = e^{-3t}$$ and we can find this solution somewhat easily with the forward Euler method shown below.
That said, by choosing a larger timestep, we see the Euler method's solution oscillate above and below 0, which should *never* happen.
If we were to take the Euler method's solution as valid, we would incorrectly assume that $$e^{-3t}$$ will become negative!

<p>
    <img  class="center" src="res/instability.png" />
</p>

Like above, the blue line is the analytical solution, the green is with a timestep of 0.5 and the red is with a timestep of 1.
Here, it's interesting that we see 2 different instability patterns.
The green is initially unstable, but converges onto the correct solution, but the red is wrong from the get-go and only gets more wrong as time goes on.

In truth, the stability region of the forward Euler method for the case where $$y' = ky$$ can be found with the following inequality:
$$
|kdt + 1 | \leq 1
$$
Which means that the forward Euler method is actually unstable for most values!
If we want to stick to using the forward Euler method exclusively, the only solution is to decrease the timestep until it is within this stability region, and that's not necessarily easy for all cases.
So now it might be obvious that another, more stable method should be used instead; however, many other stable methods are *implicit*, which means that in order to find the solution, we need to solve a system of equations via the [Thomas Algorithm](../thomas_algorithm/thomas_algorithm.md) or [Gaussian Elimination](../gaussian_elimination/gaussian_elimination.md).
Which is an entire layer of complexity that most people don't want to mess with!

Now, here is where we might want to relate the method to another algorithm that is sometimes used for a similar use-case: [Verlet Integration](../verlet_integration/verlet_integration.md).
Verlet integration has a distinct advantage over the forward Euler method in both error and stability with more coarse-grained timesteps; however, Euler methods are powerful in that they may be used for cases other than simple kinematics.
That said, in practice, due to the instability of the forward Euler method and the error with larger timesteps, this method is rarely used in practice.
That said, variations of this method *are* certainly used (for example Crank-Nicolson and [Runge-Kutta](../runge_kutta_methods/runge_kutta_methods.md), so the time spent reading this chapter is not a total waste!

## Video Explanation

Here is a video describing the forward Euler method:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/wG7h8g6VLBo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

Like in the case of [Verlet Integration](../verlet_integration/verlet_integration.md), the easiest way to test to see if this method works is to test it against a simple test-case.
Here, the most obvious test-case would be dropping a ball from 5 meters, which is my favorite example, but proved itself to be slightly less enlightening than I would have thought.
So, this time, let's remove ourselves from any physics and instead solve the following ODE: $$y(t)' = -3t$$ with the initial condition that $$y(0) = 1$$.
Note that in this case, the velocity is directly given by the ODE and the acceleration is not part of the model.

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/euler.jl)
{% sample lang="c" %}
[import, lang:"c"](code/c/euler.c)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/euler.cpp)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/euler.rs)
{% sample lang="elm" %}
[import:44-54, lang:"elm"](code/elm/euler.elm)
[import:193-210, lang:"elm"](code/elm/euler.elm)

Full code for the visualization follows:
[import, lang:"elm"](code/elm/euler.elm)

{% sample lang="py" %}
[import, lang:"python"](code/python/euler.py)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/euler.hs)
{% sample lang="m" %}
[import, lang:"matlab"](code/matlab/euler.m)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/euler.swift)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/euler.js)
{% sample lang="f90" %}
[import, lang:"fortran"](code/fortran/euler.f90)
{% sample lang="go" %}
[import, lang:"go"](code/golang/euler.go)
{% sample lang="asm-x64" %}
[import, lang:"asm-x64"](code/asm-x64/euler.s)
{% sample lang="java" %}
[import, lang:"java"](code/java/ForwardEuler.java)
{% sample lang="nim" %}
[import, lang:"nim"](code/nim/forwardeuler.nim)
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
- The image "[FEerror](res/error.png)" was created by [James Schloss](https://github.com/leios) and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The image "[FEinstability](res/instability.png)" was created by [James Schloss](https://github.com/leios) and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
