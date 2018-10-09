# Euclidean Algorithm

Computer science is (almost by definition) a science about computers -- a device first conceptualized in the 1800's. Computers have become so revolutionary, that it is difficult to think of our lives today without them. That said, *algorithms* are much older and have existed in the world for millennia. Incredibly, a few of the algorithms created before the Common Era (AD) are still in use today. One such algorithm was first described in Euclid's *Elements* (~ 300 BC) and has come to be known as the *Euclidean Algorithm*.

The algorithm is a simple way to find the *greatest common divisor* (GCD) of two numbers, which is useful for a number of different applications (like reducing fractions). The first method (envisioned by Euclid) uses simple subtraction:

{% method %}
{% sample lang="c" %}
[import:17-30, lang="c_cpp"](code/c/euclidean_example.c)
{% sample lang="cs" %}
[import:8-23, lang="csharp"](code/csharp/EuclideanAlgorithm.cs)
{% sample lang="clj" %}
[import:2-8, lang="clojure"](code/clojure/euclidean_example.clj)
{% sample lang="cpp" %}
[import:18-31, lang="c_cpp"](code/c++/euclidean.cpp)
{% sample lang="java" %}
[import:3-16, lang="java"](code/java/EuclideanAlgo.java)
{% sample lang="js" %}
[import:15-29, lang="javascript"](code/javascript/euclidean_example.js)
{% sample lang="lisp" %}
[import:3-12, lang="lisp"](code/lisp/euclidean_algorithm.lisp)
{% sample lang="py" %}
[import:11-22, lang="python"](code/python/euclidean_example.py)
{% sample lang="haskell" %}
[import:2-8, lang="haskell"](code/haskell/euclidean_example.hs)
{% sample lang="rs" %}
[import:3-15, lang="rust"](code/rust/euclidean_example.rs)
{% sample lang="ml" %}
[import:9-17, lang="ocaml"](code/ocaml/euclidean_example.ml)
{% sample lang="go" %}
[import:25-38, lang="golang"](code/go/euclidean.go)
{% sample lang="swift" %}
[import:1-14, lang="swift"](code/swift/euclidean_algorithm.swift)
{% sample lang="matlab" %}
[import:3-17, lang="matlab"](code/matlab/euclidean.m)
{% sample lang="lua" %}
[import:1-14, lang="lua"](code/lua/euclidean.lua)
{% sample lang="jl" %}
[import:12-25, lang="julia"](code/julia/euclidean.jl)
{% sample lang="nim" %}
[import:13-24, lang="nim"](code/nim/euclid_algorithm.nim)
{% sample lang="f90" %}
[import:1-19, lang="fortran"](code/fortran/euclidean.f90)
{% sample lang="php" %}
[import:4-18, lang="php"](code/php/euclidean.php)
{% sample lang="factor" %}
[import:1-13, lang="factor"](code/factor/euclid.factor)
{% sample lang="ws" %}
[import, lang="whitespace"](code/whitespace/euclidian_sub.ws)
{% sample lang="scala" %}
[import:3-8, lang="scala"](code/scala/euclidean.scala)
{% sample lang="racket" %}
[import:3-14, lang="lisp"](code/racket/euclidean_algorithm.rkt)
{% sample lang="ruby" %}
[import:8-19, lang="ruby"](code/ruby/euclidean.rb)
{% sample lang="st" %}
[import:1-13, lang="smalltalk"](code/smalltalk/euclid.st)
{% sample lang="lolcode" %}
[import:25-40, lang="LOLCODE"](code/lolcode/euclid.lol)
{% endmethod %}

Here, we simply line the two numbers up every step and subtract the lower value from the higher one every timestep. Once the two values are equal, we call that value the greatest common divisor. A graph of `a` and `b` as they change every step would look something like this:

<p>
    <img  class="center" src="res/subtraction.png" width="500" />
</p>

Modern implementations, though, often use the modulus operator (%) like so

{% method %}
{% sample lang="c" %}
[import:4-16, lang="c_cpp"](code/c/euclidean_example.c)
{% sample lang="cs" %}
[import:25-39, lang="csharp"](code/csharp/EuclideanAlgorithm.cs)
{% sample lang="clj" %}
[import:9-13, lang="clojure"](code/clojure/euclidean_example.clj)
{% sample lang="cpp" %}
[import:5-15, lang="c_cpp"](code/c++/euclidean.cpp)
{% sample lang="java" %}
[import:18-26, lang="java"](code/java/EuclideanAlgo.java)
{% sample lang="js" %}
[import:1-13, lang="javascript"](code/javascript/euclidean_example.js)
{% sample lang="lisp" %}
[import:13-17, lang="lisp"](code/lisp/euclidean_algorithm.lisp)
{% sample lang="py" %}
[import:1-9, lang="python"](code/python/euclidean_example.py)
{% sample lang="haskell" %}
[import:10-14, lang="haskell"](code/haskell/euclidean_example.hs)
{% sample lang="rs" %}
[import:17-27, lang="rust"](code/rust/euclidean_example.rs)
{% sample lang="ml" %}
[import:3-7, lang="ocaml"](code/ocaml/euclidean_example.ml)
{% sample lang="go" %}
[import:14-23, lang="golang"](code/go/euclidean.go)
{% sample lang="swift" %}
[import:16-27, lang="swift"](code/swift/euclidean_algorithm.swift)
{% sample lang="matlab" %}
[import:19-31, lang="matlab"](code/matlab/euclidean.m)
{% sample lang="lua" %}
[import:16-25, lang="lua"](code/lua/euclidean.lua)
{% sample lang="jl" %}
[import:1-10, lang="julia"](code/julia/euclidean.jl)
{% sample lang="nim" %}
[import:1-11, lang="nim"](code/nim/euclid_algorithm.nim)
{% sample lang="f90" %}
[import:21-34, lang="fortran"](code/fortran/euclidean.f90)
{% sample lang="php" %}
[import:20-30, lang="php"](code/php/euclidean.php)
{% sample lang="factor" %}
[import:15-25, lang="factor"](code/factor/euclid.factor)
{% sample lang="ws" %}
[import, lang="whitespace"](code/whitespace/euclidian_mod.ws)
{% sample lang="scala" %}
[import:10-14, lang="scala"](code/scala/euclidean.scala)
{% sample lang="racket" %}
[import:16-24, lang="lisp"](code/racket/euclidean_algorithm.rkt)
{% sample lang="ruby" %}
[import:1-6, lang="ruby"](code/ruby/euclidean.rb)
{% sample lang="st" %}
[import:15-25, lang="smalltalk"](code/smalltalk/euclid.st)
{% sample lang="lolcode" %}
[import:9-23, lang="LOLCODE"](code/lolcode/euclid.lol)
{% endmethod %}

Here, we set `b` to be the remainder of `a%b` and `a` to be whatever `b` was last timestep. Because of how the modulus operator works, this will provide the same information as the subtraction-based implementation, but when we show `a` and `b` as they change with time, we can see that it might take many fewer steps:

<p>
    <img  class="center" src="res/modulus.png" width="500" />
</p>

The Euclidean Algorithm is truly fundamental to many other algorithms throughout the history of computer science and will definitely be used again later. At least to me, it's amazing how such an ancient algorithm can still have modern use and appeal. That said, there are still other algorithms out there that can find the greatest common divisor of two numbers that are arguably better in certain cases than the Euclidean algorithm, but the fact that we are discussing Euclid two millennia after his death shows how timeless and universal mathematics truly is. I think that's pretty cool.

## Example Code

{% method %}
{% sample lang="c" %}
[import, lang="c_cpp"](code/c/euclidean_example.c)
{% sample lang="cs" %}
##### EuclideanAlgorithm.cs
[import, lang="csharp"](code/csharp/EuclideanAlgorithm.cs)
##### Program.cs
[import, lang="csharp"](code/csharp/Program.cs)
{% sample lang="clj" %}
[import, lang="clojure"](code/clojure/euclidean_example.clj)
{% sample lang="cpp" %}
[import, lang="c_cpp"](code/c++/euclidean.cpp)
{% sample lang="java" %}
[import, lang="java"](code/java/EuclideanAlgo.java)
{% sample lang="js" %}
[import, lang="javascript"](code/javascript/euclidean_example.js)
{% sample lang="lisp" %}
[import, lang="lisp"](code/lisp/euclidean_algorithm.lisp)
{% sample lang="py" %}
[import, lang="python"](code/python/euclidean_example.py)
{% sample lang="haskell" %}
[import, lang="haskell"](code/haskell/euclidean_example.hs)
{% sample lang="rs" %}
[import, lang="rust"](code/rust/euclidean_example.rs)
{% sample lang="ml" %}
[import, lang="ocaml"](code/ocaml/euclidean_example.ml)
{% sample lang="go" %}
[import, lang="golang"](code/go/euclidean.go)
{% sample lang="swift" %}
[import, lang="swift"](code/swift/euclidean_algorithm.swift)
{% sample lang="matlab" %}
[import, lang="matlab"](code/matlab/euclidean.m)
{% sample lang="lua" %}
[import, lang="lua"](code/lua/euclidean.lua)
{% sample lang="jl" %}
[import, lang="julia"](code/julia/euclidean.jl)
{% sample lang="nim" %}
[import, lang="nim" %](code/nim/euclid_algorithm.nim)
{% sample lang="f90" %}
[import, lang="fortran"](code/fortran/euclidean.f90)
{% sample lang="php" %}
[import, lang="php"](code/php/euclidean.php)
{% sample lang="factor" %}
[import, lang="factor"](code/factor/euclid.factor)
{% sample lang="ws" %}
Here is a readable version of the algorithms with comments. First, subtraction method:
[import, lang="whitespace"](code/whitespace/euclidian_sub_comments.ws)
and modulo method:
[import, lang="whitespace"](code/whitespace/euclidian_mod_comments.ws)
{% sample lang="scala" %}
[import, lang="scala"](code/scala/euclidean.scala)
{% sample lang="racket" %}
[import, lang="lisp"](code/racket/euclidean_algorithm.rkt)
{% sample lang="ruby" %}
[import, lang="ruby"](code/ruby/euclidean.rb)
{% sample lang="st" %}
[import, lang="smalltalk"](code/smalltalk/euclid.st)
{% sample lang="lolcode" %}
[import, lang="LOLCODE"](code/lolcode/euclid.lol)
{% endmethod %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
