### Quick sort 
 Quick sort is one of the most important sorting methods in javascript. It takes a pivot value(a random value) from an array. All the other elements in the array are split to two
categories.They may be less than the pivot value and greater than the pivot value.After that each of the categories(less than the pivot and greater than the pivot) are subjected 
to the same procedure that is a pivot is chosen then each category is divided in to sub-categories(less than the pivot and greater than the pivot).Eventually, the sub-categories
are divided in such a way that they may contain an element or no element if there are no more elements to compare. The rest of the values will be denoted as a pivots at some
previous points and did not trickle down to this lowest sub category.


There are two basic operations in the algorithm, swapping items in place and partitioning a section of the array. The basic steps to partition an array are:

 1. Find a “pivot” item in the array. This item is the basis for comparison for a single round.
 2. Start a pointer (the left pointer) at the first item in the array.
 3. Start a pointer (the right pointer) at the last item in the array.
 4. While the value at the left pointer in the array is less than the pivot value,
    move the left pointer to the right (add 1). Continue until the value at the left pointer is greater than or equal to the pivot value.
 5. While the value at the right pointer in the array is greater than the pivot value, move the right pointer to the left (subtract 1).
    Continue until the value at the right pointer is less than or equal to the pivot value.
 6. If the left pointer is less than or equal to the right pointer, then swap the values at these locations in the array.
 7. Move the left pointer to the right by one and the right pointer to the left by one.
 9. If the left pointer and right pointer don’t meet, go to step 1.
 
{% method %}
{% sample lang="js" %}
[import:1-11, lang:"javascript"](code/javascript/quicksort.js)
{% endmethod %}


{% method %}
{% sample lang="js" %}
[import:13-29, lang:"javascript"](code/javascript/quickort.js)
{% endmethod %}


This is an example of how merge sort works over every iteration until return the result.

<pre>
    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

</pre>

## Example Code

{% method %}
{% sample lang="js" %}
[import:31-38, lang:"javascript"](Code/javascript/quicksort.js)
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
