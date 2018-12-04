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
[import:3-12, lang="lisp"](code/clisp/euclidean_algorithm.lisp)
{% sample lang="py" %}
[import:11-22, lang="python"](code/python/euclidean_example.py)
{% sample lang="haskell" %}
[import:2-8, lang="haskell"](code/haskell/euclidean_example.hs)
{% sample lang="rs" %}
[import:3-15, lang="rust"](code/rust/euclidean_example.rs)
{% sample lang="ml" %}
[import:9-17, lang="ocaml"](code/ocaml/euclidean_example.ml)
{% sample lang="go" %}
[import:25-38, lang="go"](code/go/euclidean.go)
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
{% sample lang="asm-x64" %}
[import:35-56, lang="asm-x64"](code/asm-x64/euclidean_example.s)
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
{% sample lang="emojic" %}
[import:2-17, lang:"emojicode"](code/emojicode/euclidean_algorithm.emojic)
{% sample lang="lolcode" %}
[import:25-40, lang="LOLCODE"](code/lolcode/euclid.lol)
{% sample lang="bash" %}
[import:24-38, lang="bash"](code/bash/euclid.bash)
{% sample lang="bf" %}
[import, lang="brainfuck"](code/brainfuck/euclidean_sub.bf)
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
[import:13-17, lang="lisp"](code/clisp/euclidean_algorithm.lisp)
{% sample lang="py" %}
[import:1-9, lang="python"](code/python/euclidean_example.py)
{% sample lang="haskell" %}
[import:10-14, lang="haskell"](code/haskell/euclidean_example.hs)
{% sample lang="rs" %}
[import:17-27, lang="rust"](code/rust/euclidean_example.rs)
{% sample lang="ml" %}
[import:3-7, lang="ocaml"](code/ocaml/euclidean_example.ml)
{% sample lang="go" %}
[import:14-23, lang="go"](code/go/euclidean.go)
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
{% sample lang="asm-x64" %}
[import:10-33, lang="asm-x64"](code/asm-x64/euclidean_example.s)
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
{% sample lang="emojic" %}
[import:19-31, lang:"emojicode"](code/emojicode/euclidean_algorithm.emojic)
{% sample lang="lolcode" %}
[import:9-23, lang="LOLCODE"](code/lolcode/euclid.lol)
{% sample lang="bash" %}
[import:10-22, lang="bash"](code/bash/euclid.bash)
{% sample lang="bf" %}
[import, lang="brainfuck"](code/brainfuck/euclidean_mod.bf)
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
[import, lang="lisp"](code/clisp/euclidean_algorithm.lisp)
{% sample lang="py" %}
[import, lang="python"](code/python/euclidean_example.py)
{% sample lang="haskell" %}
[import, lang="haskell"](code/haskell/euclidean_example.hs)
{% sample lang="rs" %}
[import, lang="rust"](code/rust/euclidean_example.rs)
{% sample lang="ml" %}
[import, lang="ocaml"](code/ocaml/euclidean_example.ml)
{% sample lang="go" %}
[import, lang="go"](code/go/euclidean.go)
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
{% sample lang="asm-x64" %}
[import, lang="asm-x64"](code/asm-x64/euclidean_example.s)
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
{% sample lang="emojic" %}
[import, lang:"emojicode"](code/emojicode/euclidean_algorithm.emojic)
{% sample lang="lolcode" %}
[import, lang="LOLCODE"](code/lolcode/euclid.lol)
{% sample lang="bash" %}
[import, lang="bash"](code/bash/euclid.bash)
{% sample lang="bf" %}
#### Subtraction varient
##### Code
[import, lang="brainfuck"](code/brainfuck/euclidean_sub.bf)
##### State of memory after each chunk of code:

```
[a (b) 0 0 1 0 0]
[a b a b (0) 0 0]
if(a>b) [a b a-b 0 (a-b) 0 0]
else [a b 0 b-a (b-a) 0 0]
if(a-b==0)
    print and break
else 
    if(a>b) [a-b b 0 0 (a-b) 0 0]
    else [a b-a 0 0 (b-a) 0 0]
And the codeis looped till cell 0 = cell 1
```

##### Explanation

**Here we just get the input from the user:**

`scan a,b`: `>,>,`
State: `a (b) 0 0 0 0 0`

**The input is duplicated**
```
>>>+
[
[-]<<<
<[->>>+<<<]>[->>>+<<<]>>
```
State: `0 0 0 (a) b 0 0`
```
[-<+<<+>>>]
>[-<+<<+>>>]
```
State: `a b a b (0) 0 0`

**Here, we subtract 1 from cell 2 from 3 until one of them hits 0, the other cell would be |a-b|**
```
<<
[->- subtracts a from b, assuming a>b
>[-]+
<[
>-]>
[->>]<<< if a is 0, stop
]
```
`>[>]` moves the pointer to cell 4

Now the states are
`a b a-b 0 (0) 0`
or
`a b 0 b-a (0) 0`


**If cell 3 is non-zero, let cell 1=cell 4=cell 3**
```
<[<<[-]>>[-<<+>>>+<]]
```
**If cell 2 is non-zero, let cell 0=cell 4=cell 2**
```
<[<<[-]>>[-<<+>>>>+<<]]
```

State: `a b 0 b-a (0) 0 -> a b-a (0) 0 b-a 0`
       `a b a-b 0 (0) 0 -> a-b b (0) 0 a-b 0`

This tests if cell 4 is zero, if so print cell 0 and break
```
>>>[-]+
>[-]<<
[>-]>
[<<<<.>>> testing if difference is 0, if so return
>->>]<<
]
```

#### Modulo varient
##### Code
[import, lang="brainfuck"](code/brainfuck/euclidean_mod.bf)
##### Explanation
`scan a,b`: `>,>,`

State: `0 a >b 0 0 0`

`while(b!=0)`: `[`

`a,b,0=0,b-a%b,a%b`:
```
<[
   >->+<[>]
   >[<+>-]<
   <[<]>-
]
```

so basically this is the modulo algorithm in brainfuck, it slowly shifts cell 2 to cell 3, while subtracting 1 from cell 1
then when cell 2 goes to 0, it shifts cell 3 to 2 and continues, this is like just constantly subtracting cell 2 from cell 1, until you cant subtract anymore then return at cell 3

State: `0 >0 b-a%b a%b 0 0`

shifting: `>[-<+>]`

State: `0 b-a%b >0 a%b 0 0`

Currently we have a,b,0=b-a%b,0,a%b, and we need a,b,0=b,a%b,0, so we just add the third cell to the first and second cell

adding thing: `>[-<+<+>>]<`

State: `0 b >(a%b) 0 0 0`

So now we have a,b=b,a%b, we continue the loop

`]`

After the second cell is 0, the loop terminates and we obtain the GCD

State: `0 >GCD(a b) 0 0 0 0`

Now we print the GCD

print: `<.`
{% endmethod %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

