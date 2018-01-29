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

# Bogo Sort
Look, Bogo Sort doesn't really sort anything out. 
In fact, it should never be used in practice for any reason I can think of and really only serves as a joke in the programming community.
As far as jokes go, though, this one's pretty good.

So, here's how it goes:
imagine you have an array of $$n$$ elements that you want sorted.
One way to do it is to shuffle the array at random and hope that all the elements will be magically in order after shuffling.
If they are not in order, just shuffle everything again.
And then again. And again.
In the best case, this algorithm runs with a complexity of $$\Omega(1)$$, and in the worst, $$\mathcal{O}(\infty)$$.

In code, it looks something like this:

{% method %}
{% sample lang="pseudo" %}
```julia
function bogo_sort(Vector{Type} a)
    while(!is_sorted(a))
        shuffle(a)
    end
end
```
{% sample lang="cs" %}
[import:29-35, unindent=true, lang:"csharp"](code/Sorting.cs)
{% endmethod %}

That's it.
Ship it!
We are done here!
