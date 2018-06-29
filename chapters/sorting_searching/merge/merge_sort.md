# Merge Sort

In computer science, merge sort (also commonly spelled mergesort) is an efficient, general-purpose, comparison-based sorting algorithm.
Most implementations produce a stable sort, which means that the implementation preserves the input order of equal elements in the sorted output.
Merge sort is a divide and conquer algorithm that was invented by John von Neumann in 1945.
A detailed description and analysis of bottom-up mergesort appeared in a report by Goldstine and von Neumann as early as 1948.

Conceptually, a merge sort works as follows:
    1. Divide the unsorted list into n sublists, each containing 1 element (a list of 1 element is considered sorted).
    2. Repeatedly merge sublists to produce new sorted sublists until there is only 1 sublist remaining. This will be the sorted list.

{% method %}
{% sample lang="py" %}
[import:1-40, lang:"python"](code/python/merge.py)
{% endmethod %}

This algorithim can be made faster by doing the merge operations in parallel.

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
