# Monte Carlo Integration

Monte Carlo methods were some of the first methods I ever used for research, and when I learned about them, they seemed like some sort of magic.
Their premise is simple: random numbers can be used to integrate arbitrary shapes embedded into other objects.
Nowadays, "Monte Carlo" has become a bit of a catch-all term for methods that use random numbers to produce real results, but it all started as a straightforward method to integrate objects.
No matter how you slice it, the idea seems a bit crazy at first.
After all, random numbers are random.
How could they possibly be used to find non-random values?

Well, imagine you have a square.
The area of the square is simple, $$\text{Area}_{\text{square}} = \text{length} \times \text{width}$$.
Since it's a square, the $$\text{length}$$ and $$\text{width}$$ are the same, so the formula is technically just $$\text{Area}_{\text{square}} = \text{length}^2$$.
If we embed a circle into the square with a radius $$r = \tfrac{length}{2}$$ (shown below), then its area is $$\text{Area}_{\text{circle}}=\pi r^2$$.
For simplicity, we can also say that $$\text{Area}_{\text{square}}=4r^2$$.

<p>
    <img  class="center" src="res/square_circle.png" width="300"/>
</p>

Now, let's say we want to find the area of the circle without an equation.
As we said before, it's embedded in the square, so we should be able to find some ratio of the area of the square to the area of the circle:

$$
\text{Ratio} = \frac{\text{Area}_{\text{circle}}}{\text{Area}_{\text{square}}}
$$

This means,

$$
\text{Area}_{\text{circle}} = \text{Area}_{\text{square}}\times\text{Ratio} = 4r^2 \times \text{ratio}
$$

So, if we can find the $$\text{Ratio}$$ and we know $$r$$, we should be able to easily find the $$\text{Area}_{\text{circle}}$$.
The question is, "How do we easily find the $$\text{Ratio}$$?"
Well, one way is with *random sampling*.
We basically just pick a bunch of points randomly in the square, and
each point is tested to see whether it's in the circle or not:

{% method %}
{% sample lang="jl" %}
[import:2-7, lang:"julia"](code/julia/monte_carlo.jl)
{% sample lang="clj" %}
[import:3-10, lang:"clojure"](code/clojure/monte_carlo.clj)
{% sample lang="c" %}
[import:7-9, lang:"c"](code/c/monte_carlo.c)
{% sample lang="cpp" %}
[import:7-16, lang:"cpp"](code/c++/monte_carlo.cpp)
{% sample lang="js" %}
[import:2-6, lang:"javascript"](code/javascript/monte_carlo.js)
{% sample lang="hs" %}
[import:7-7, lang:"haskell"](code/haskell/monteCarlo.hs)
{% sample lang="rs" %}
[import:7-9, lang:"rust"](code/rust/monte_carlo.rs)
{% sample lang="d" %}
[import:2-5, lang:"d"](code/d/monte_carlo.d)
{% sample lang="go" %}
[import:12-14, lang:"go"](code/go/monteCarlo.go)
{% sample lang="r" %}
[import:2-6, lang:"r"](code/r/monte_carlo.R)
{% sample lang="java" %}
[import:12-14, lang:"java"](code/java/MonteCarlo.java)
{% sample lang="swift" %}
[import:1-3, lang:"swift"](code/swift/monte_carlo.swift)
{% sample lang="py" %}
[import:5-7, lang:"python"](code/python/monte_carlo.py)
{% sample lang="cs" %}
[import:23-23, lang:"csharp"](code/csharp/Circle.cs)
{% sample lang="nim" %}
[import:6-7, lang:"nim"](code/nim/monte_carlo.nim)
{% sample lang="ruby" %}
[import:1-4, lang:"ruby"](code/ruby/monte_carlo.rb)
{% sample lang="f90" %}
[import:1-8, lang:"fortran"](code/fortran/monte_carlo.f90)
{% sample lang="factor" %}
[import:9-12 lang:"factor"](code/factor/monte_carlo.factor)
{% sample lang="emojic" %}
[import:23-27, lang:"emojicode"](code/emojicode/monte_carlo.emojic)
{% sample lang="php" %}
[import:4-7, lang:"php"](code/php/monte_carlo.php)
{% sample lang="lua" %}
[import:1-3, lang="lua"](code/lua/monte_carlo.lua)
{% sample lang="racket" %}
[import:2-4, lang:"lisp"](code/racket/monte_carlo.rkt)
{% sample lang="scala" %}
[import:3-3, lang:"scala"](code/scala/monte_carlo.scala)
{% sample lang="lisp" %}
[import:3-5, lang:"lisp"](code/clisp/monte-carlo.lisp)
{% sample lang="asm-x64" %}
[import:21-32, lang:"asm-x64"](code/asm-x64/monte_carlo.s)
{% sample lang="bash" %}
[import:2-10, lang:"bash"](code/bash/monte_carlo.bash)
{% sample lang="kotlin" %}
[import:3-3, lang:"kotlin"](code/kotlin/MonteCarlo.kt)
{% endmethod %}

If it's in the circle, we increase an internal count by one, and in the end,

$$
\text{Ratio} = \frac{\text{count in circle}}{\text{total number of points used}}
$$

If we use a small number of points, this will only give us a rough approximation, but as we start adding more and more points, the approximation becomes much, much better (as shown below)!

<p>
    <img  class="center" src="res/monte_carlo.gif" width="400"/>
</p>

The true power of Monte Carlo comes from the fact that it can be used to integrate literally any object that can be embedded into the square.
As long as you can write some function to tell whether the provided point is inside the shape you want (like `in_circle()` in this case), you can use Monte Carlo integration!
This is obviously an incredibly powerful tool and has been used time and time again for many different areas of physics and engineering.
I can guarantee that we will see similar methods crop up all over the place in the future!

## Video Explanation

Here is a video describing Monte Carlo integration:

<div style="text-align:center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/AyBNnkYrSWY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Example Code
Monte Carlo methods are famous for their simplicity.
It doesn't take too many lines to get something simple going.
Here, we are just integrating a circle, like we described above; however, there is a small twist and trick.
Instead of calculating the area of the circle, we are instead trying to find the value of $$\pi$$, and
rather than integrating the entire circle, we are only integrating the upper right quadrant of the circle from $$0 < x, y < 1$$.
This saves a bit of computation time, but also requires us to multiply our output by $$4$$.

That's all there is to it!
Feel free to submit your version via pull request, and thanks for reading!

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/monte_carlo.jl)
{% sample lang="clj" %}
[import, lang:"clojure"](code/clojure/monte_carlo.clj)
{% sample lang="c" %}
[import, lang:"c"](code/c/monte_carlo.c)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/monte_carlo.cpp)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/monte_carlo.js)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/monteCarlo.hs)
{%sample lang="rs" %}
[import, lang:"rust"](code/rust/monte_carlo.rs)
{%sample lang="d" %}
[import, lang:"d"](code/d/monte_carlo.d)
{%sample lang="go" %}
[import, lang:"go"](code/go/monteCarlo.go)
{%sample lang="r" %}
[import, lang:"r"](code/r/monte_carlo.R)
{% sample lang="java" %}
[import, lang:"java"](code/java/MonteCarlo.java)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/monte_carlo.swift)
{% sample lang="py" %}
[import, lang:"python"](code/python/monte_carlo.py)
{% sample lang="cs" %}
##### MonteCarlo.cs
[import, lang:"csharp"](code/csharp/MonteCarlo.cs)
##### Circle.cs
[import, lang:"csharp"](code/csharp/Circle.cs)
##### Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
{% sample lang="nim" %}
[import, lang:"nim"](code/nim/monte_carlo.nim)
{% sample lang="ruby" %}
[import, lang:"ruby"](code/ruby/monte_carlo.rb)
{% sample lang="f90" %}
[import, lang:"fortran"](code/fortran/monte_carlo.f90)
{% sample lang="factor" %}
[import, lang:"factor"](code/factor/monte_carlo.factor)
{% sample lang="emojic" %}
[import, lang:"emojicode"](code/emojicode/monte_carlo.emojic)
{% sample lang="php" %}
[import, lang:"php"](code/php/monte_carlo.php)
{% sample lang="lua" %}
[import, lang="lua"](code/lua/monte_carlo.lua)
{% sample lang="racket" %}
[import, lang:"lisp"](code/racket/monte_carlo.rkt)
{% sample lang="scala" %}
[import, lang:"scala"](code/scala/monte_carlo.scala)
{% sample lang="lisp" %}
[import, lang:"lisp"](code/clisp/monte-carlo.lisp)
{% sample lang="asm-x64" %}
[import, lang:"asm-x64"](code/asm-x64/monte_carlo.s)
{% sample lang="bash" %}
[import, lang:"bash"](code/bash/monte_carlo.bash)
{% sample lang="kotlin" %}
[import, lang:"kotlin"](code/kotlin/MonteCarlo.kt)
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
- The image "[squarecircle](res/square_circle.png)" was created by [James Schloss](https://github.com/leios) and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The animation "[simplemontecarlo](res/monte_carlo.gif)" was created by [James Schloss](https://github.com/leios) and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).


##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
