# Tree Traversal

Trees are naturally recursive data structures, and because of this, we cannot access their elements like we might access the elements of a vector or array. Instead, we need to use more interesting methods to work through each element. This is often called *Tree Traversal*, and there are many different ways to do this. For now, we will restrict the discussion to two common and related methods of tree traversal: *Depth-First* and *Breadth-First Search*. Note that trees vary greatly in shape and size depending on how they are used; however, they are composed primarily of nodes that house other, children nodes, like so:

{% method %}
{% sample lang="jl" %}
[import:3-7, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import:15-18, lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
[import:7-11, lang:"csharp"](code/csharp/Tree.cs)
{% sample lang="c" %}
[import:7-11, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
[import:110-126, lang:"java"](code/java/Tree.java)
{% sample lang="js" %}
This has not been implemented in your chosen language, so here is the Julia code
[import:3-7, lang:"julia"](code/julia/Tree.jl)
{% sample lang="py" %}
[import:1-4, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}
<p>
  <img  class="center" src="code/scratch/struct.svg" width="250" />
</p>
{% sample lang="rs"%}
[import:4-7, lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
[import:1-3, lang:"haskell"](code/haskell/TreeTraversal.hs)
{% sample lang="swift"%}
[import:1-9, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import:3-27, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import:1-5, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}

Because of this, the most straightforward way to traverse the tree might be recursive. This naturally leads us to the Depth-First Search (DFS) method:

{% method %}
{% sample lang="jl" %}
[import:9-16, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import:20-27, lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
[import:34-45, lang:"csharp"](code/csharp/Tree.cs)
{% sample lang="c" %}
[import:37-45, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
[import:21-27, lang:"java"](code/java/Tree.java)
{% sample lang="js" %}
[import:12-15, lang:"javascript"](code/javascript/tree.js)
{% sample lang="py" %}
[import:18-23, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}
<p>
  <img  class="center" src="code/scratch/dfs.svg" width="250" />
  <img  class="center" src="code/scratch/dfs-from.svg" width="250" />
</p>
{% sample lang="rs"%}
[import:9-15 lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
[import:5-6, lang:"haskell"](code/haskell/TreeTraversal.hs)
{% sample lang="swift"%}
[import:24-30, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import:31-35, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import:7-10, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}

At least to me, this makes a lot of sense. We fight recursion with recursion! First, we first output the node we are on and then we call `DFS_recursive(...)` on each of its children nodes. This method of tree traversal does what its name implies: it goes to the depths of the tree first before going through the rest of the branches. In this case, the ordering looks like:

<p>
    <img  class="center" src="res/DFS_pre.png" width="500" />
</p>

Note that the in the code above, we are missing a crucial step: *checking to see if the node we are using actually exists!* Because we are using a vector to store all the nodes, we will be careful not to run into a case where we call `DFS_recursive(...)` on a node that has yet to be initialized; however, depending on the language we are using, we might need to be careful of this to avoid recursion errors!

Now, in this case the first element searched through is still the root of the tree. This type of tree traversal is known as *pre-order* DFS. We perform an action (output the ID) *before* searching through the children. If we shift the function around and place the data output at the end of the function, we can modify the order in which we search through the tree to be *post-order* and look something like this:


{% method %}
{% sample lang="jl" %}
[import:18-26, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import:29-34 lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
[import:47-58, lang:"csharp"](code/csharp/Tree.cs)
{% sample lang="c" %}
[import:47-53, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
[import:34-41, lang:"java"](code/java/Tree.java)
{% sample lang="js" %}
[import:17-20, lang:"javascript"](code/javascript/tree.js)
{% sample lang="py" %}
[import:26-31, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}
<p>
  <img  class="center" src="code/scratch/dfs-post.svg" width="300" />
</p>
{% sample lang="rs"%}
[import:17-23, lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
[import:8-9, lang:"haskell"](code/haskell/TreeTraversal.hs)
{% sample lang="swift"%}
[import:32-38, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import:37-41, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import:12-15, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}

<p>
    <img  class="center" src="res/DFS_post.png" width="500" />
</p>

In this case, the first node visited is at the bottom of the tree and moves up the tree branch by branch. In addition to these two types, binary trees have an *in-order* traversal scheme that looks something like this:

{% method %}
{% sample lang="jl" %}
[import:28-43, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import:37-55 lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
[import:60-79, lang:"csharp"](code/csharp/Tree.cs)
{% sample lang="c" %}
[import:55-73, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
[import:48-62, lang:"java"](code/java/Tree.java)
{% sample lang="js" %}
[import:22-34, lang:"javascript"](code/javascript/tree.js)
{% sample lang="py" %}
[import:34-46, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}
<p>
  <img  class="center" src="code/scratch/dfs-in.svg" width="300" />
</p>
{% sample lang="rs"%}
[import:25-38, lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
[import:11-15, lang:"haskell"](code/haskell/TreeTraversal.hs)
{% sample lang="swift"%}
[import:40-53, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import:43-62, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import:17-31, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}

<p>
    <img  class="center" src="res/DFS_in.png" width="500" />
</p>


The order here seems to be some mix of the other 2 methods and works through the binary tree from left to right.

Now, at this point, it might seem that the only way to search through a recursive data structure is with recursion, but this is not necessarily the case! Rather surprisingly, we can perform a DFS non-recursively by using a stack, which are data structures that hold multiple elements, but only allow you to interact with the very last element you put in. The idea here is simple:

1. Put the root node in the stack
2. Take it out and put in its children
3. Pop the top of the stack and put its children in
4. Repeat 3 until the stack is empty

In code, it looks like this:

{% method %}
{% sample lang="jl" %}
[import:45-56, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import:58-73, lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
[import:81-94, lang:"csharp"](code/csharp/Tree.cs)
{% sample lang="c" %}
[import:75-93, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
[import:65-79, lang:"java"](code/java/Tree.java)
{% sample lang="js" %}
[import:36-43, lang:"javascript"](code/javascript/tree.js)
{% sample lang="py" %}
[import:49-60, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}
<p>
  <img  class="center" src="code/scratch/dfs-stack.svg" width="400" />
</p>
{% sample lang="rs"%}
[import:40-47, lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
This has not been implemented in your chosen language, so here is the Julia code
[import:45-56, lang:"julia"](code/julia/Tree.jl)
{% sample lang="swift"%}
[import:55-67, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import:64-73, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import:33-41, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}

All this said, there are a few details about DFS that might not be idea, depending on the situation. For example, if we use DFS on an incredibly long tree, we will spend a lot of time going further and further down a single branch without searching the rest of the data structure. In addition, it is not the natural way humans would order a tree if asked to number all the nodes from top to bottom. I would argue a more natural traversal order would look something like this:

<p>
    <img  class="center" src="res/BFS_simple.png" width="500" />
</p>

And this is exactly what Breadth-First Search (BFS) does! On top of that, it can be implemented in the same way as the `DFS_stack(...)` function above, simply by swapping the `stack` for a `queue`, which is similar to a stack, except that it only allows you to interact with the very first element instead of the last. In code, this looks something like:

{% method %}
{% sample lang="jl" %}
[import:58-69, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import:76-89, lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
[import:96-109, lang:"csharp"](code/csharp/Tree.cs)
{% sample lang="c" %}
[import:95-113, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
[import:81-95, lang:"java"](code/java/Tree.java)
{% sample lang="js" %}
[import:45-52, lang:"javascript"](code/javascript/tree.js)
{% sample lang="py" %}
[import:75-84, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}
<p>
  <img  class="center" src="code/scratch/bfs.svg" width="400" />
</p>
{% sample lang="rs"%}
[import:49-57, lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
[import:17-20, lang:"haskell"](code/haskell/TreeTraversal.hs)
{% sample lang="swift"%}
[import:69-81, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import:65-74, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import:43-51, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}

## Example Code
{% method %}
{% sample lang="jl" %}
[import, lang:"julia"](code/julia/Tree.jl)
{% sample lang="cpp" %}
[import, lang:"c_cpp"](code/c++/tree_example.cpp)
{% sample lang="cs" %}
##### Tree.cs
[import, lang:"csharp"](code/csharp/Tree.cs)
##### Program.cs
[import, lang:"csharp"](code/csharp/Program.cs)
{% sample lang="c" %}
##### utility.h
[import, lang:"c_cpp"](code/c/utility.h)
##### tree_traversal.c
[import, lang:"c_cpp"](code/c/tree_traversal.c)
{% sample lang="java" %}
##### Tree.java
[import, lang:"java"](code/java/Tree.java)
##### MainClass.java
[import, lang:"java"](code/java/MainClass.java)
{% sample lang="js" %}
[import, lang:"javascript"](code/javascript/tree.js)
{% sample lang="py" %}
[import, lang:"python"](code/python/Tree_example.py)
{% sample lang="scratch" %}

The code snippets were taken from this [Scratch project](https://scratch.mit.edu/projects/174017753/)

{% sample lang="rs"%}
[import, lang:"rust"](code/rust/tree.rs)
{% sample lang="hs"%}
[import, lang:"haskell"](code/haskell/TreeTraversal.hs)
{% sample lang="swift"%}
[import, lang:"swift"](code/swift/tree.swift)
{% sample lang="php"%}
[import, lang:"php"](code/php/tree_traversal.php)
{% sample lang="crystal" %}
[import, lang:"crystal"](code/crystal/tree-traversal.cr)
{% endmethod %}


<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
