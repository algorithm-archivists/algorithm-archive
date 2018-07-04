# Insertion Sort
Insertion sort is the first algorithm usually taught in an introductory algorithms course since it is simple to understand as it is used regulary to sort decks of cards. Insertion sort has one important rule which is helpful to keep in mind, in the j-th iteration the subarray A[1...j-1] is sorted, it means that all the elements which the algorithm iterated over, are sorted.

The algorithm starts from the second element in the array, in this point the subarray A[1...j-1], which is A[1], is obviously sorted, since it holds only one element. 

{% method %}
{% sample lang="c" %}
[import:19-20, lang:"c"](code/c/insertion_sort.c)
{% sample lang="py" %}
[import:5-6, lang:"python"](code/python/insertion_sort.py)
{% endmethod %}

In each iteration the current , j-th, element of A, A[j], is inserted into the correct position in the subarray A[1...j], moving each element that is bigger than A[j] one position to the right, leaving room for A[j].

{% method %}
{% sample lang="c" %}
[import:24-29, lang:"c"](code/c/insertion_sort.c)
{% sample lang="py" %}
[import:10-15, lang:"python"](code/python/insertion_sort.py)
{% endmethod %}


The worst input for insertion sort is the reverse sorted array, since in each iteration of the inner while loop will iterate over the entire A[1...j-1] array, moving each element one position to the right, this is why it has time complexity of $$\mathcal{O}(n^2)$$.

And the full code:
{% method %}
{% sample lang="c" %}
[import:14-42, lang:"c"](code/python/insertion_sort.c)
{% sample lang="py" %}
[import:1-24, lang:"python"](code/python/insertion_sort.py)
{% endmethod %}

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