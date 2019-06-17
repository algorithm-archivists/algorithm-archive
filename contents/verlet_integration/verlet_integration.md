# Verlet Integration

Verlet integration is essentially a solution to the kinematic equation for the motion of any object,

$$
x = x_0 + v_0t + \frac{1}{2}at^2 + \frac{1}{6}bt^3 + \cdots
$$

where $$x$$ is the position, $$v$$ is the velocity, $$a$$ is the acceleration, $$b$$ is the often forgotten jerk term, and $$t$$ is time. This equation is a central equation to almost every Newtonian physics solver and brings up a class of algorithms known as *force integrators*. One of the first force integrators to work with is *Verlet Integration*.

So, let's say we want to solve for the next timestep in $$x$$. To a close approximation (actually performing a Taylor Series Expansion about $$x(t\pm \Delta t)$$), that might look like this:

$$
x(t+\Delta t) = x(t) + v(t)\Delta t + \frac{1}{2}a(t)\Delta t^2 + \frac{1}{6}b(t) \Delta t^3 + \mathcal{O}(\Delta t^4)
$$

This means that if we need to find the next $$x$$, we need the current $$x$$, $$v$$, $$a$$, etc. However, because few people calculate the jerk term, our error is typically $$\mathcal{O}(\Delta t^3)$$. That said, we can calculate $$x$$ with less knowledge and higher accuracy if we play a trick! Let's say we want to calculate $$x$$ of the *previous* timestep. Again, to a close approximation, that might look like this:

$$
x(t-\Delta t) = x(t) - v(t)\Delta t + \frac{1}{2}a(t)\Delta t^2 - \frac{1}{6}b(t) \Delta t^3 + \mathcal{O}(\Delta t^4)
$$

Now, we have two equations to solve for two different timesteps in x, one of which we already have. If we add the two equations together and solve for $$x(t+\Delta t)$$, we find that

$$
x(t+ \Delta t) = 2x(t) - x(t-\Delta t) + a(t)\Delta t^2 + \mathcal{O}(\Delta t^4)
$$

So, this means we can find our next $$x$$ simply by knowing our current $$x$$, the $$x$$ before that, and the acceleration! No velocity necessary! In addition, this drops the error to $$\mathcal{O}(\Delta t^4)$$, which is great!
Here is what it looks like in code:

{% method %}
{% sample lang="jl" %}
[import:1-13, lang:"julia"](code/julia/verlet.jl)
{% sample lang="cpp" %}
[import:9-22, lang:"cpp"](code/c++/verlet.cpp)
{% sample lang="c" %}
[import:3-14, lang:"c"](code/c/verlet.c)
{% sample lang="java" %}
[import:2-17, lang:"java"](code/java/Verlet.java)
{% sample lang="py" %}
[import:1-10, lang:"python"](code/python/verlet.py)
{% sample lang="hs" %}
[import:14-21, lang:"haskell"](code/haskell/verlet.hs)
{% sample lang="scratch" %}
Unfortunately, this has not yet been implemented in scratch, so here's Julia code:
[import:1-13, lang:"julia"](code/julia/verlet.jl)
{% sample lang="matlab" %}
Unfortunately, this has not yet been implemented in matlab, so here's Julia code:
[import:1-13, lang:"julia"](code/julia/verlet.jl)
{% sample lang="LabVIEW" %}
Unfortunately, this has not yet been implemented in LabVIEW, so here's Julia code:
[import:1-13, lang:"julia"](code/julia/verlet.jl)
{% sample lang="javascript" %}
[import:1-14, lang:"javascript"](code/javascript/verlet.js)
{% sample lang="rs" %}
[import:1-13, lang:"rust"](code/rust/verlet.rs)
{% sample lang="swift" %}
[import:1-15, lang:"swift"](code/swift/verlet.swift)
{% sample lang="f90" %}
[import:1-20, lang:"fortran"](code/fortran/verlet.f90)
{% sample lang="ruby" %}
[import:1-14, lang="ruby"](code/ruby/verlet.rb)
{% sample lang="go" %}
[import:5-16, lang:"go"](code/golang/verlet.go)
{% sample lang="asm-x64" %}
[import:18-42, lang:"asm-x64"](code/asm-x64/verlet.s)
{% sample lang="kotlin" %}
[import:3-15, lang:"kotlin"](code/kotlin/verlet.kt)
{% sample lang="nim" %}
[import:1-14, lang:"nim"](code/nim/verlet.nim)
{% endmethod %}

Now, obviously this poses a problem; what if we want to calculate a term that requires velocity, like the kinetic energy, $$\frac{1}{2}mv^2$$? In this case, we certainly cannot get rid of the velocity! Well, we can find the velocity to $$\mathcal{O}(\Delta t^2)$$ accuracy by using the Stormer-Verlet method, which is the same as before, but we calculate velocity like so

$$
v(t) = \frac{x(t+\Delta t) - x(t-\Delta t)}{2\Delta t} + \mathcal{O}(\Delta t^2)
$$

Note that the 2 in the denominator appears because we are going over 2 timesteps. It's essentially solving $$v=\frac{\Delta x}{\Delta t}$$. In addition, we can calculate the velocity of the next timestep like so

$$
v(t+\Delta t) = \frac{x(t+\Delta t) - x(t)}{\Delta t} + \mathcal{O}(\Delta t)
$$

However, the error for this is $$\mathcal{O}(\Delta t)$$, which is quite poor, but it gets the job done in a pinch.  Here's what it looks like in code:

{% method %}
{% sample lang="jl" %}
[import:15-31, lang:"julia"](code/julia/verlet.jl)
{% sample lang="cpp" %}
[import:24-41, lang:"cpp"](code/c++/verlet.cpp)
{% sample lang="c" %}
[import:16-31, lang:"c"](code/c/verlet.c)
{% sample lang="java" %}
[import:19-37, lang:"java"](code/java/Verlet.java)
{% sample lang="py" %}
[import:12-23, lang:"python"](code/python/verlet.py)
{% sample lang="hs" %}
[import:23-28, lang:"haskell"](code/haskell/verlet.hs)
{% sample lang="scratch" %}
Unfortunately, this has not yet been implemented in scratch, so here's Julia code:
[import:15-31, lang:"julia"](code/julia/verlet.jl)
{% sample lang="matlab" %}
Unfortunately, this has not yet been implemented in matlab, so here's Julia code:
[import:15-31, lang:"julia"](code/julia/verlet.jl)
{% sample lang="LabVIEW" %}
Unfortunately, this has not yet been implemented in LabVIEW, so here's Julia code:
[import:15-31, lang:"julia"](code/julia/verlet.jl)
{% sample lang="javascript" %}
[import:16-32, lang:"javascript"](code/javascript/verlet.js)
{% sample lang="rs" %}
[import:15-32, lang:"rust"](code/rust/verlet.rs)
{% sample lang="swift" %}
[import:17-34, lang:"swift"](code/swift/verlet.swift)
{% sample lang="f90" %}
[import:22-42, lang:"fortran"](code/fortran/verlet.f90)
{% sample lang="ruby" %}
[import:16-32, lang="ruby"](code/ruby/verlet.rb)
{% sample lang="go" %}
[import:18-30, lang:"go"](code/golang/verlet.go)
{% sample lang="asm-x64" %}
[import:44-71, lang:"asm-x64"](code/asm-x64/verlet.s)
{% sample lang="kotlin" %}
[import:17-30, lang:"kotlin"](code/kotlin/verlet.kt)
{% sample lang="nim" %}
[import:16-32, lang:"nim"](code/nim/verlet.nim)
{% endmethod %}


Now, let's say we actually need the velocity to calculate out next timestep. Well, in this case, we simply cannot use the above approximation and instead need to use the *Velocity Verlet* algorithm.

# Velocity Verlet

In some ways, this algorithm is even simpler than above. We can calculate everything like

$$
\begin{align}
x(t+\Delta t) &=x(t) + v(t)\Delta t + \frac{1}{2}a(t)\Delta t^2 \\
a(t+\Delta t) &= f(x(t+\Delta t)) \\
v(t+\Delta t) &= v(t) + \frac{1}{2}(a(t) + a(t+\Delta t))\Delta t
\end{align}
$$

which is literally the kinematic equation above, solving for $$x$$, $$v$$, and $$a$$ every timestep. You can also split up the equations like so

$$
\begin{align}
v(t+\frac{1}{2}\Delta t) &= v(t) + \frac{1}{2}a(t)\Delta t \\
x(t+\Delta t) &=x(t) + v(t+\frac{1}{2}\Delta t)\Delta t \\
a(t+\Delta t) &= f(x(t+\Delta t)) \\
v(t+\Delta t) &= v(t+\frac{1}{2}\Delta t) + \frac{1}{2}a(t+\Delta t)\Delta t
\end{align}
$$

Here is the velocity Verlet method in code:

{% method %}
{% sample lang="jl" %}
[import:33-45, lang:"julia"](code/julia/verlet.jl)
{% sample lang="cpp" %}
[import:43-54, lang:"cpp"](code/c++/verlet.cpp)
{% sample lang="c" %}
[import:33-43, lang:"c"](code/c/verlet.c)
{% sample lang="java" %}
[import:39-51, lang:"java"](code/java/Verlet.java)
{% sample lang="py" %}
[import:25-34, lang:"python"](code/python/verlet.py)
{% sample lang="hs" %}
[import:30-35, lang:"haskell"](code/haskell/verlet.hs)
{% sample lang="scratch" %}
Unfortunately, this has not yet been implemented in scratch, so here's Julia code:
[import:33-45, lang:"julia"](code/julia/verlet.jl)
{% sample lang="matlab" %}
Unfortunately, this has not yet been implemented in matlab, so here's Julia code:
[import:33-45, lang:"julia"](code/julia/verlet.jl)
{% sample lang="LabVIEW" %}
Unfortunately, this has not yet been implemented in LabVIEW, so here's Julia code:
[import:33-45, lang:"julia"](code/julia/verlet.jl)
{% sample lang="javascript" %}
[import:34-45, lang:"javascript"](code/javascript/verlet.js)
{% sample lang="rs" %}
[import:34-45, lang:"rust"](code/rust/verlet.rs)
{% sample lang="swift" %}
[import:36-49, lang:"swift"](code/swift/verlet.swift)
{% sample lang="f90" %}
[import:44-60, lang:"fortran"](code/fortran/verlet.f90)
{% sample lang="ruby" %}
[import:34-46, lang="ruby"](code/ruby/verlet.rb)
{% sample lang="go" %}
[import:32-42, lang:"go"](code/golang/verlet.go)
{% sample lang="asm-x64" %}
[import:73-101, lang:"asm-x64"](code/asm-x64/verlet.s)
{% sample lang="kotlin" %}
[import:32-42, lang:"kotlin"](code/kotlin/verlet.kt)
{% sample lang="nim" %}
[import:34-45, lang:"nim"](code/nim/verlet.nim)
{% endmethod %}

Even though this method is more widely used than the simple Verlet method mentioned above, it unforunately has an error term of $$\mathcal{O}(\Delta t^2)$$, which is two orders of magnitude worse. That said, if you want to have a simulaton with many objects that depend on one another --- like a gravity simulation --- the Velocity Verlet algorithm is a handy choice; however, you may have to play further tricks to allow everything to scale appropriately. These types of simulatons are sometimes called *n-body* simulations and one such trick is the Barnes-Hut algorithm, which cuts the complexity of n-body simulations from $$\sim \mathcal{O}(n^2)$$ to $$\sim \mathcal{O}(n\log(n))$$.

## Video Explanation

Here is a video describing Verlet integration:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/g55QvpAev0I" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Example Code

Both of these methods work simply by iterating timestep-by-timestep and can be written straightforwardly in any language. For reference, here are snippets of code that use both the classic and velocity Verlet methods to find the time it takes for a ball to hit the ground after being dropped from a given height.

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/verlet.jl)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/verlet.cpp)
{% sample lang="c" %}
[import, lang:"c"](code/c/verlet.c)
{% sample lang="java" %}
[import, lang:"java"](code/java/VerletValues.java)
[import, lang:"java"](code/java/Verlet.java)
{% sample lang="py" %}
[import, lang:"python"](code/python/verlet.py)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/verlet.hs)
{% sample lang="scratch" %}
Submitted by Jie
<p>
    <img  class="center" src="code/scratch/verlet_scratch.png" />
</p>
Link: [https://scratch.mit.edu/projects/173039394/](https://scratch.mit.edu/projects/173039394/)
{% sample lang="matlab" %}
[import, lang:"matlab"](code/matlab/verlet.m)
{% sample lang="LabVIEW" %}
Submitted by P. Mekhail
<p>
    <img  class="center" src="code/labview/verlet_labview.png" />
</p>
{% sample lang="javascript" %}
[import, lang:"javascript"](code/javascript/verlet.js)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/verlet.rs)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/verlet.swift)
{% sample lang="f90" %}
[import, lang:"fortran"](code/fortran/verlet.f90)
{% sample lang="ruby" %}
[import, lang="ruby"](code/ruby/verlet.rb)
{% sample lang="go" %}
[import, lang:"go"](code/golang/verlet.go)
{% sample lang="asm-x64" %}
[import, lang:"asm-x64"](code/asm-x64/verlet.s)
{% sample lang="kotlin" %}
[import, lang:"kotlin"](code/kotlin/verlet.kt)
{% sample lang="nim" %}
[import, lang="nim"](code/nim/verlet.nim)
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

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
