# Bogo Sort
Look, Bogo Sort doesn't really sort anything out.
In fact, it should never be used in practice for any reason I can think of and really only serves as a joke in the programming community.
As far as jokes go, though, this one's pretty good.

So, here's how it goes:
imagine you have an array of $$n$$ elements that you want sorted.
One way to do it is to shuffle the array at random and hope that all the elements will be magically in order after shuffling.
If they are not in order, just shuffle everything again.
And then again. And again.
In the best case, this algorithm runs with a complexity of $$\Omega(n)$$, and in the worst, $$\mathcal{O}(\infty)$$.

In code, it looks something like this:

{% method %}
{% sample lang="jl" %}
[import:12-16, lang:"julia"](code/julia/bogo.jl)
{% sample lang="cs" %}
[import:9-15, lang:"csharp"](code/csharp/BogoSort.cs)
{% sample lang="clj" %}
[import:7-11, lang:"clojure"](code/clojure/bogo.clj)
{% sample lang="c" %}
[import:25-29, lang:"c"](code/c/bogo_sort.c)
{% sample lang="java" %}
[import:2-6, lang:"java"](code/java/Bogo.java)
{% sample lang="js" %}
[import:11-15, lang:"javascript"](code/javascript/bogo.js)
{% sample lang="py" %}
[import:10-12, lang:"python"](code/python/bogo.py)
{% sample lang="hs" %}
[import:17-20, lang:"haskell"](code/haskell/bogoSort.hs)
{% sample lang="m" %}
[import:21-28, lang:"matlab"](code/matlab/bogosort.m)
{% sample lang="lua" %}
[import:1-22, lang="lua"](code/lua/bogosort.lua)
{% sample lang="cpp" %}
[import:33-38, lang:"cpp"](code/c++/bogosort.cpp)
{% sample lang="rs" %}
[import:16-20, lang:"rust"](code/rust/bogosort.rs)
{% sample lang="swift" %}
[import:13-19, lang:"swift"](code/swift/bogosort.swift)
{% sample lang="php" %}
[import:15-22, lang:"php"](code/php/bogo_sort.php)
{% sample lang="nim" %}
[import:16-18, lang:"nim"](code/nim/bogo_sort.nim)
{% sample lang="ruby" %}
[import:12-16, lang:"ruby"](code/ruby/bogo.rb)
{% sample lang="emojic" %}
[import:2-6, lang:"emojicode"](code/emojicode/bogo_sort.emojic)
{% sample lang="factor" %}
[import:10-12, lang:"factor"](code/factor/bogo_sort.factor)
{% sample lang="f90" %}
[import:24-32, lang:"fortran"](code/fortran/bogo.f90)
{% sample lang="racket" %}
[import:3-8, lang:"lisp"](code/racket/bogo_sort.rkt)
{% sample lang="st" %}
[import:2-6, lang:"smalltalk"](code/smalltalk/bogosort.st)
{% sample lang="bash" %}
[import:38-45, lang:"bash"](code/bash/bogo_sort.bash)
{% sample lang="asm-x64" %}
[import:93-113, lang:"asm-x64"](code/asm-x64/bogo_sort.s)
{% sample lang="lisp" %}
[import:20-24, lang:"lisp"](code/clisp/bogo-sort.lisp)
{% sample lang="crystal" %}
[import:10-14, lang:"crystal"](code/crystal/bogo.cr)
{% sample lang="r" %}
[import:1-6, lang:"r"](code/r/bogo_sort.r)
{% sample lang="scala" %}
[import:12-16, lang:"scala"](code/scala/bogo.scala)
{% sample lang="go" %}
[import:27-31, lang:"go"](code/go/bogo_sort.go)
{% endmethod %}

That's it.
Ship it!
We are done here!

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/bogo.jl)
{% sample lang="cs" %}
##### BogoSort.cs
[import, lang:"csharp"](code/csharp/BogoSort.cs)
##### Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
{% sample lang="clj" %}
[import, lang:"clojure"](code/clojure/bogo.clj)
{% sample lang="c" %}
[import, lang:"c"](code/c/bogo_sort.c)
{% sample lang="java" %}
[import, lang:"java"](code/java/Bogo.java)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/bogo.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/bogo.py)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bogoSort.hs)
{% sample lang="m" %}
[import, lang:"matlab"](code/matlab/bogosort.m)
{% sample lang="lua" %}
[import, lang="lua"](code/lua/bogosort.lua)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/bogosort.cpp)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/bogosort.rs)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/bogosort.swift)
{% sample lang="php" %}
[import, lang:"php"](code/php/bogo_sort.php)
{% sample lang="nim" %}
[import, lang:"nim"](code/nim/bogo_sort.nim)
{% sample lang="ruby" %}
[import, lang:"ruby"](code/ruby/bogo.rb)
{% sample lang="emojic" %}
[import, lang:"emojicode"](code/emojicode/bogo_sort.emojic)
{% sample lang="factor" %}
[import, lang:"factor"](code/factor/bogo_sort.factor)
{% sample lang="f90" %}
[import, lang:"fortran"](code/fortran/bogo.f90)
{% sample lang="racket" %}
[import, lang:"lisp"](code/racket/bogo_sort.rkt)
{% sample lang="st" %}
[import, lang:"smalltalk"](code/smalltalk/bogosort.st)
{% sample lang="bash" %}
[import, lang:"bash"](code/bash/bogo_sort.bash)
{% sample lang="asm-x64" %}
[import, lang:"asm-x64"](code/asm-x64/bogo_sort.s)
{% sample lang="lisp" %}
[import, lang:"lisp"](code/clisp/bogo-sort.lisp)
{% sample lang="crystal" %}
[import, lang:"crystal"](code/crystal/bogo.cr)
{% sample lang="r" %}
[import, lang:"r"](code/r/bogo_sort.r)
{% sample lang="scala" %}
[import, lang:"scala"](code/scala/bogo.scala)
{% sample lang="go" %}
[import, lang:"go"](code/go/bogo_sort.go)
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
