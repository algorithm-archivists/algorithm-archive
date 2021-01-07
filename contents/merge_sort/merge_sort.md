# Merge Sort
Merge sort or mergesort is one of the most efficient and popular sorting algorithms today, it was invented by John von Neumann in 1945. It works on the principle of divide and conquer. That means, it will divide the problem into smaller problems and then solve each of these small problems in order to solve the whole problem. In other words, merge sort works by dividing the unordered array of elements into two halves and sorting them, which in turn it will split again in two halves and sort (recursively),  then all the results will be merged resulting in a sorted array.

Merge sort guarantees to sort an array of N items in time proportional to $$\mathcal{O}(nLogn)$$, no matter what the input. It’s good to keep in mind that it works better with larger amounts of data than small sets, in which case should be considered another algorithm, like insertion sort. Another caracteritics is that it's a stable sort which means that the same element in an array maintain their original positions with respect to each other.

How does it work? This implementation is known as top-down implementation, the array is split into two parts that are used to call mergesort recursively. The result of these calls is merged into a sorted array and returned to the upper call.

{% method %}
{% sample lang="js" %}
[import:1-11, lang:"javascript"](code/javascript/mergesort.js)
{% endmethod %}

The merge part of the algorithm is responsible of go over the two parts to fill up the resulting array with the elements sorted.

{% method %}
{% sample lang="js" %}
[import:13-29, lang:"javascript"](code/javascript/mergesort.js)
{% endmethod %}

This is an example of how merge sort works over every iteration until return the result.

<pre>
    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
    ----------------------------------------------
 0: M  E  R  G  E  S  O  R  T  E  X  A  M  P  L  E
 1: E  M
 2:       G  R
 3: E  G  M  R
 4:             E  S
 5:                   O  R
 6:             E  O  R  S
 7: E  E  G  M  O  R  R  S
 8:                         E  T
 9:                               A  X
10:                         A  E  T  X
11:                                     M  P
12:                                           E  L
13:                                     E  L  M  P
14:                         A  E  E  L  M  P  T  X
15: A  E  E  E  E  G  L  M  M  O  P  R  R  S  T  X
</pre>

## Example Code

{% method %}
{% sample lang="js" %}
[import:31-38, lang:"javascript"](code/javascript/mergesort.js)
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
