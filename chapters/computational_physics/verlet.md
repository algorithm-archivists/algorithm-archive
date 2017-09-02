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

* [Taylor Series Expansions](../mathematical_background/taylor_series.md)

# Verlet Integration

Verlet Integration is essentially a solution to the kinematic equation for the motion of any object, which can be found with a [Taylor Series Expansion](../mathematical_background/taylor_series.md) to be

$$
x(t) = x(0) + v(0)t + \frac{1}{2}at^2 + \frac{1}{6}bt^3 + \cdots
$$

Where $$x$$ is the position, $$v$$ is the velocity, $$a$$ is the acceleration, $$b$$ is the often forgotten jerk term, and $$t$$ is time. This equation is a central equation to almost every Newtonian physics solver and brings up a class of algorithms known as *force integratiors*. One of the first force integrators to work with is *Verlet Integration*.

So, let's say we want to solve for the next timestep in $$x$$. To a close approximation, that might look like this:

$$
x(t+\Delta t) = x(t) + v(t)\Delta t + \frac{1}{2}a\Delta t^2 + \frac{1}{6}b \Delta t^3 + \mathcal{O}(\Delta t^4)
$$

This means that if we need to find the next $$x$$, we need the current $$x$$, $$v$$, $$a$$, etc. However, because few people calculate the jerk term, our error is typically $$\mathcal{O}(\Delta t^3)$$. That said, we can calculate $$x$$ with less knowledge and higher accuracy if we play a trick! Let's say we want to calculate $$x$$ of the *previous* timestep. Again, to a close approximation, that might look like this:

$$
x(t-\Delta t) = x(t) - v(t)\Delta t + \frac{1}{2}a\Delta t^2 - \frac{1}{6}b \Delta t^3 + \mathcal{O}(\Delta t^4)
$$

Now, we have two equations to solve for two different timesteps in x, one of which we already have. If we add the two equations together and solve for $$x(t+\Delta t)$$, we find

$$
x(t+ \Delta t) = 2x(t) - x(t-\Delta t) + a\Delta t^2 + \mathcal{O}(\Delta t^4)
$$

So, this means, we can find our next $$x$$ simply by knowing our current $$x$$, the $$x$$ before that, and the acceleration! No velocity necessary! In addition, this drops the error to $$\mathcal{O}(\Delta t^4)$$, which is great!

Now, obviously this poses a problem, what if we want to calculate a term that requires velocity, like the kinetic energy, $$\frac{1}{2}mv^2$$? In this case, we certainly cannot get rid of the velocity! Well, we can find the velocity to $$\mathcal{O}(\Delta t^2)$$ accuracy by using the St\"ormer-Verlet method, which is the same as before, but we calculate velocity like so

$$
v(t) = \frac{x(t+\Delta t) - x(t-\Delta t)}{2\Delta t} + \mathcal{O}(\Delta t^2)
$$

Note that the 2 in the denominator appears because we are going over 2 timesteps. It's essentially solving $$v=\frac{\Delta x}{\Delta t}$$. In addition, we can calculate the velocity of the next timestep like so

$$
v(t+\Delta t) = \frac{x(t+\Delta t) - x(t)}{\Delta t} + \mathcal{O}(\Delta t)
$$

However, the error for this is $$\mathcal{O}(\Delta t)$$, which is quite poor, but get's the job done in a pinch.

Now, let's say we actually need the velocity to calculate out next tiemstep. Well, in this case, we simply cannot use the above approximation and instead need to use the *Velocity Verlet* algorithm.

# Velocity Verlet

In some ways, this algorithm is even simpler than above. We can calculate everything like so

$$
\begin{align}
x(t+\Delta t) &=x(t) + v(t)\Delta t + \frac{1}{2}a(t)\Delta t^2 \\
a(t+\Delta t) &= f(x(t+\Delta t)) \\
v(t+\Delta t) &= v(t) + \frac{1}{2}(a(t) + a(t+\Delta t))\Delta t
\end{align}
$$

Which is literally the kinematic equation above, solving for $$x$$, $$v$$, and $$a$$ every timestep. You can also split up the equations like so

$$
\begin{align}
v(t+\frac{1}{2}\Delta t) &= v(t) + \frac{1}{2}a(t)\Delta t \\
x(t+\Delta t) &=x(t) + v(t+\frac{1}{2}\Delta t)\Delta t \\
a(t+\Delta t) &= f(x(t+\Delta t)) \\
v(t+\Delta t) &= v(t+\frac{1}{2}\Delta t) + \frac{1}{2}a(t+\Delta t)\Delta t
\end{align}
$$

Even though this method is more used than the simple Verlet method mentioned above, it unforunately has an error term of $$\mathcal{O} \Delta t^2$$, which is two orders of magnitude worse. That said, if you want to have a simulaton with many objects that depend on one another --- like a gravity simulation --- the Velocity Verlet algorithm is a handy choice; however, you may have to play further tricks to allow everything to scale appropriately. These types of simulatons are sometimes called *n-body* simulations and one such trick is the [Barnes-Hut](barnes_hut.md) algorithm, which cuts the complexity of n-body simulations from $$\sim \mathcal{O}(n^2)$$ to $$\sim \mathcal{O}(n\log(n))$$

# Example Code

Both of these methods work simply by iterating timestep-by-timestep and can be written straightforwardly in any language. For reference, here are snippets of code that use both the classic and velocity Verlet methods to find the time it takes for a ball to hit the ground after being dropped from a given height.

#### C++
```c++
/*------------verlet.cpp------------------------------------------------------//
*
* Purpose: Simple demonstration of verlet algorithm
*
*-----------------------------------------------------------------------------*/

#include <iostream>

// Simple function for velocity-verlet
void verlet(double pos, double acc, double dt){

    // Note that we are using a temp variable for the previous position
    double prev_pos, temp_pos, time;
    prev_pos = pos;
    time = 0;

    while (pos > 0){
        time += dt;
        temp_pos = pos;
        pos = pos*2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

   std::cout << time << '\n';

}

// Simple function for stormer-verlet
void stormer_verlet(double pos, double acc, double dt){

    // Note that we are using a temp variable for the previous position
    double prev_pos, temp_pos, time, vel;
    prev_pos = pos;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        temp_pos = pos;
        pos = pos*2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;

        // The acceleration is constant, so the velocity is straightforward
        vel += acc*dt;
    }

   std::cout << time << '\n';

}

// Simple function for velocity-verlet
void velocity_verlet(double pos, double acc, double dt){

    // Note that we are using a temp variable for the previous position
    double time, vel;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        pos += vel*dt + 0.5*acc * dt * dt;
        vel += acc*dt;
    }

   std::cout << time << '\n';

}

int main(){

    verlet(5.0, -10, 0.01);
    stormer_verlet(5.0, -10, 0.01);
    velocity_verlet(5.0, -10, 0.01);

}

```
