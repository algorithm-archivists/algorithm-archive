## What Makes a Fourier Transform Fast?

If there were ever an algorithm to radically change the landscape of computer science and engineering by making seemingly impossible possible, it would be the Fast Fourier Transform (FFT). On the surface, the algorithm seems like a simple application of recursion, and in principle, that is exactly what it is; however, the Fourier Transform is no ordinary transform -- it allows researchers and engineers to easily bounce back and forth between real space and frequency space. 

Now, I know this seems underwhelming, just a neat trick to show friends on a Sunday night when everyone's bored, but the sheer number of engineering applications that use frequency space is overwhelming! From calculating superfluid vortex positions to super-resolution imaging, Fourier Transforms lie at the heart of many scientific disciplines and are essential to many algorithms we will cover later in this book. 

Simply put, the Fourier Transform is a beautiful application of complex number systems; however, it would never be used today if not for the ability to quickly perform the operation through the use of the Fast Fourier Transform, first introduced by the great Frederick Gauss in 1805 and later independently discovered by James Cooley and John Tukey in 1965{{ "ct1965" | cite }}. Gauss (of course) already had too many things named after him and Cooley and Tukey both had cooler names, so the most common algorithm for FFT's today is known as the Cooley-Tukey algorithm.

### What is a Fourier Transform?

To an outsider, the Fourier Transform looks like a mathematical mess -- certainly a far cry from the heroic portal between two domains I have depicted it to be; however, like most things, it's not as bad as it initially appears to be. So, here it is in all it's glory!

$$F(\xi) = \int_{-\infty} ^\infty f(x) e^{-2 \pi i x \xi} dx$$

and

$$f(x) = \int_{-\infty} ^\infty F(\xi) e^{2 \pi i \xi x} d\xi$$

Where $$F(\xi)$$ represents a function in frequency space and $$\xi$$ represents any number on the frequency plane, and $$f(x)$$ represents any number in real space and $$x$$ represents any value on the real plane. Note here that the only difference between the two exponential terms is a minus sign in the transformation to frequency space. As I mentioned, this is not intuitive syntax, so please allow me to explain a bit.

If we take a sinusoidal function like $$\sin(\omega t)$$ or $$\cos(\omega t)$$, we find a curve that goes from $$\pm1$$, shown in FIGURE1a. Both of these curves can be described by their corresponding frequencies, $$\omega$$. So, instead of representing these curves as seen in FIGURE1a, We could instead describe them as shown in FIGURE1b. Here, FIGRE1a is in real space and FIGURE1b is in frequency space. 

Now, how does this relate to the transformations above? Well, the easiest way is to substitute in the following relation:

$$e^{2 \pi i \theta} = \cos(2 \pi \theta) + i \sin(2 \pi \theta)$$

This clearly turns our function in Frequency space into:

$$F(\xi) = \int_{-\infty} ^\infty f(x) (\cos(-2 \pi x \xi) + i \sin(-2 \pi x \xi))dx$$

and our function in real space into:

$$f(x) = \int_{-\infty} ^\infty F(\xi) (\cos(2 \pi \xi x) + i \sin(2 \pi \xi x))e^{2 \pi i \xi x} d\xi$$

Here, there are $$\sin$$ and $$\cos$$ functions in the formulas, so it looks much friendlier, right? This means that a point in real space is defined by the integral over all space of it's corresponding frequency function multiplied by sinusoidal oscillations. At this point, mathematicians usually get it. Truth be told, I didn't. In fact, I didn't understand it fully until we discretized real and frequency space to create the Discrete Fourier Transform (DFT).

### What is a Discrete Fourier Transform?

In principle, the Discrete Fourier Transform (DFT) is simply the Fourier transform with summations instead of integrals:

$$X_k = \sum_{n=0}^{N-1} x_n \cdot e^{-2 \pi k n / N}$$

and 

$$x_n = \frac{1}{N} \sum_{k=0}^{N-1} X_k \cdot e^{2 \pi k n / N}$$

Where $$X_n$$ and $$x_n$$ are sequences of $$N$$ complex and real numbers, respectively. In principle, this is no easier to understand than the previous case! For some reason, though, putting code to this transformation really helped me figure out what was actually going on.

```
function DFT(x::Array{Float64})
    N = length(x)

    # We want two vectors here for real space (n) and frequency space (k)
    n = 0:N-1
    k = n'
    transform_matrix = exp(-2im * pi *n *k / N)
    return x * transform_matrix

end
```

In this function, we define `n` to be a set of integers from $$0 \rightarrow N-1$$ and arrange them to be a column. We then set `k` to be the same thing, but in a row. This means that when we multiply them together, we get a matrix, but not just any matrix! This matrix is the heart to the transformation itself!

```
transform_matrix = [1.0+0.0im  1.0+0.0im           1.0+0.0im          1.0+0.0im; 
                    1.0+0.0im  6.12323e-17-1.0im  -1.0-1.22465e-16im -1.83697e-16+1.0im; 
                    1.0+0.0im -1.0-1.22465e-16im   1.0+2.44929e-16im -1.0-3.67394e-16im; 
                    1.0+0.0im -1.83697e-16+1.0im  -1.0-3.67394e-16im  5.51091e-16-1.0im]
```

It was amazing to me when I saw the transform for what it truly was: an actual transformation matrix! That said, the Discrete Fourier Transform is slow -- primarily because matrix multiplication is slow, and as mentioned before, slow code is not particularly useful. So what was the trick that everyone used to go from a Discrete fourier Transform to a *Fast* Fourier Transform? 

Recursion!

### The Cooley-Tukey Algorithm

In some sense, I may have oversold this algorithm. In fact, I definitely have. It's like I have already given you the punchline to a joke and am now getting around to explaining it. Oh well.

### Butterfly Diagrams

### Bibliography

{% references %} {% endreferences %}
