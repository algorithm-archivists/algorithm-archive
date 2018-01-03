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

# Graham Scan

At around the same time of the [Jarvis March](jarvis_march.md), R. L. Graham was also developing an algorithm to find the convex hull of a random set of points {{ "gs1972" | cite }}.
Unlike the Jarvis March, which is an $$\mathcal{O}(nh)$$ operation, the Graham Scan is $$\mathcal{O}(n\log(n))$$, where $$n$$ is the number of points and $$h$$ is the size fo the hull. 
This means that the complexity of the Graham Scan is not output-sensitive; moreover, there are some cases where the Jarvis March is more optimal, depending on the size of the hull and the number of points to wrap.

Rather than starting at the leftmost point like the Jarvis March, the Graham scan starts at the bottom. 
We then sort the distribution of points based on the angle between the bottom-most point, the origin, and each other point.
After sorting, we go through point-by-point, searching for points that are on the convex hull and throwing out any other points.
We do this by looking for counter-clockwise rotations.
If an angle between three points turns inward, the shape is obviously not convex, so we can throw that result out.
We can find whether a rotation is counter-clockwise with trigonometric functions or by using a cross-product, like so:

```julia
type Point
    Float64 x,y
end

function ccw(Point a, Point b, Point c)
    return (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)
end
```

If the output of this function is 0, the points are collinear.
If the output is positive, then the points form a counter-clockwise "left" turn.
If the output is negative, then the points form a clockwise "right" turn.
We basically do not want clockwise rotations, because this means we are at an interior angle.

<---ADD FIGURE--->

To save memory and expensive `append()` operations, we ultimately look for points that should be on the hull and swap them with the first elements in the array.
If there are $$M$$ elements on the hull, then the first $$M$$ elements in our output random distribution of points will be the hull. 
In the end, the code should look something like this:

```julia
function graham_scan(Vector{Point} points)
    N = length(points)

    # Place the lowest point at the start of the array
    swap(points[0], lowest(points[0]))
    
    # Sort according to angle with that point
    # Note: Not sure how to show lambda functions in psuedocode...
    sort(points, points[c] -> angle({0,0}, points[0], c)

    # M will be the point on the hull
    Int64 M = 1
    for i = 2:N
        while (ccw(points[M-1], points[M], points[i]) <= 0)
            # Removing the ccw point from the hull by skipping its index
            if M > 1
                M -= 1
            # All points are collinear
            else if (i == N)
                break
            else 
                i++
            end
        end

        # ccw point found, updating hull and swapping points
        M++
        swap(points[i], points[m])
    end
end
```

### Bibliography

{% references %} {% endreferences %}

### Example Code

To be added later!


