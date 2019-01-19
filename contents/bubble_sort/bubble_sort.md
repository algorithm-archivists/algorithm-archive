# Bubble Sort
When it comes to sorting algorithms, Bubble Sort is usually the first that comes to mind.
Though it might not be the fastest tool in the shed, it's definitely straightforward to implement and is often the first sorting method new programmers think of when trying to implement a sorting method on their own.

Here's how it works: we go through each element in our vector and check to see if it is larger than the element to it's right.
If it is, we swap the elements and then move to the next element.
In this way, we sweep through the array $$n$$ times for each element and continually swap any two adjacent elements that are improperly ordered.
This means that we need to go through the vector $$\mathcal{O}(n^2)$$ times with code similar to the following:

{% method %}
{% sample lang="jl" %}
[import:1-10, lang:"julia"](code/julia/bubble.jl)
{% sample lang="cs" %}
[import:9-27, lang:"csharp"](code/csharp/BubbleSort.cs)
{% sample lang="c" %}
[import:10-20, lang:"c"](code/c/bubble_sort.c)
{% sample lang="c8" %}
[import:39-63, lang:"chip-8"](code/chip8/bubblesort.c8)
{% sample lang="java" %}
[import:2-12, lang:"java"](code/java/Bubble.java)
{% sample lang="kotlin" %}
[import:1-11, lang:"kotlin"](code/kotlin/BubbleSort.kt)
{% sample lang="js" %}
[import:1-12, lang:"javascript"](code/javascript/bubble.js)
{% sample lang="py" %}
[import:4-9, lang:"python"](code/python/bubblesort.py)
{% sample lang="m" %}
[import:1-13, lang:"matlab"](code/matlab/bubblesort.m)
{% sample lang="lua" %}
[import:1-9, lang="lua"](code/lua/bubble_sort.lua)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bubbleSort.hs)
{% sample lang="cpp" %}
[import:13-23, lang:"cpp"](code/c++/bubblesort.cpp)
{% sample lang="rs" %}
[import:6-16, lang:"rust"](code/rust/bubble_sort.rs)
{% sample lang="d" %}
[import:3-18, lang:"d"](code/d/bubble_sort.d)
{% sample lang="go" %}
[import:7-21, lang:"go"](code/go/bubbleSort.go)
{% sample lang="racket" %}
[import:6-19, lang:"scheme"](code/racket/bubbleSort.rkt)
{% sample lang="swift" %}
[import:1-13, lang:"swift"](code/swift/bubblesort.swift)
{% sample lang="ti83b" %}
[import:2-13, lang:"ti-83_basic"](code/ti83basic/BUBLSORT.txt)
{% sample lang="ruby" %}
[import:3-13, lang:"ruby"](code/ruby/bubble.rb)
{% sample lang="crystal" %}
[import:1-11, lang:"crystal"](code/crystal/bubble.cr)
{% sample lang="php" %}
[import:4-17, lang:"php"](code/php/bubble_sort.php)
{% sample lang="lisp" %}
[import:3-28, lang:"lisp"](code/clisp/bubble_sort.lisp)
{% sample lang="nim" %}
[import:5-9, lang:"nim"](code/nim/bubble_sort.nim)
{% sample lang="st" %}
[import:2-15, lang:"smalltalk"](code/smalltalk/bubble.st)
{% sample lang="asm-x64" %}
[import:43-66, lang:"asm-x64"](code/asm-x64/bubble_sort.s)
{% sample lang="f90" %}
[import:19-40, lang:"fortran"](code/fortran/bubble.f90)
{% sample lang="bf" %}
[import:17-63, lang:"brainfuck"](code/brainfuck/bubblesort.bf)
{% sample lang="scala" %}
[import:3-14, lang:"scala"](code/scala/bubble_sort.scala)
{% sample lang="emojic" %}
[import:2-14, lang:"emojicode"](code/emojicode/bubble_sort.emojic)
{% sample lang="bash" %}
[import:2-21, lang:"bash"](code/bash/bubble_sort.bash)
{% endmethod %}

... And that's it for the simplest bubble sort method.
Now, as you might imagine, computer scientists have optimized this to the fiery lakes of Michigan and back, so we'll come back to this in the future and talk about how to optimize it.
For now, it's fine to just bask in the simplicity that is bubble sort.
Trust me, there are plenty of more complicated algorithms that do precisely the same thing, only much, much better (for most cases).

## Example Code

{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/bubble.jl)
{% sample lang="cs" %}
##### BubbleSort.cs
[import, lang:"csharp"](code/csharp/BubbleSort.cs)
##### Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
{% sample lang="c" %}
[import, lang:"c"](code/c/bubble_sort.c)
{% sample lang="c8" %}
[import, lang:"chip-8"](code/chip8/bubblesort.c8)
{% sample lang="java" %}
[import, lang:"java"](code/java/Bubble.java)
{% sample lang="kotlin" %}
[import, lang:"kotlin"](code/kotlin/BubbleSort.kt)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/bubble.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/bubblesort.py)
{% sample lang="m" %}
[import, lang:"matlab"](code/matlab/bubblesort.m)
{% sample lang="lua" %}
[import, lang="lua"](code/lua/bubble_sort.lua)
{% sample lang="hs" %}
[import, lang:"haskell"](code/haskell/bubbleSort.hs)
{% sample lang="cpp" %}
[import, lang:"cpp"](code/c++/bubblesort.cpp)
{% sample lang="rs" %}
[import, lang:"rust"](code/rust/bubble_sort.rs)
{% sample lang="d" %}
[import, lang:"d"](code/d/bubble_sort.d)
{% sample lang="go" %}
[import, lang:"go"](code/go/bubbleSort.go)
{% sample lang="racket" %}
[import, lang:"scheme"](code/racket/bubbleSort.rkt)
{% sample lang="swift" %}
[import, lang:"swift"](code/swift/bubblesort.swift)
{% sample lang="ti83b" %}
[import, lang:"ti-83_basic"](code/ti83basic/BUBLSORT.txt)
{% sample lang="ruby" %}
[import, lang:"ruby"](code/ruby/bubble.rb)
{% sample lang="crystal" %}
[import, lang:"crystal"](code/crystal/bubble.cr)
{% sample lang="php" %}
[import, lang:"php"](code/php/bubble_sort.php)
{% sample lang="lisp" %}
[import, lang:"lisp"](code/clisp/bubble_sort.lisp)
{% sample lang="nim" %}
[import, lang:"nim"](code/nim/bubble_sort.nim)
{% sample lang="asm-x64" %}
[import, lang:"asm-x64"](code/asm-x64/bubble_sort.s)
{% sample lang="f90" %}
[import, lang:"fortran"](code/fortran/bubble.f90)
{% sample lang="bf" %}
[import, lang:"brainfuck"](code/brainfuck/bubblesort.bf)
{% sample lang="st" %}
[import, lang:"smalltalk"](code/smalltalk/bubble.st)
{% sample lang="scala" %}
[import, lang:"scala"](code/scala/bubble_sort.scala)
{% sample lang="emojic" %}
[import, lang:"emojicode"](code/emojicode/bubble_sort.emojic)
{% sample lang="bash" %}
[import, lang:"bash"](code/bash/bubble_sort.bash)
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
