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

## What Makes a Fourier Transform Fast?

If there were ever an algorithm to radically change the landscape of computer science and engineering by making seemingly impossible problems possible, it would be the Fast Fourier Transform (FFT). 
On the surface, the algorithm seems like a simple application of recursion, and in principle, that is exactly what it is; however, the Fourier Transform is no ordinary transform -- it allows researchers and engineers to easily bounce back and forth between real space and frequency space and is the heart of many physics and engineering applications.
From calculating superfluid vortex positions to super-resolution imaging, Fourier Transforms lay at the heart of many scientific disciplines and are essential to many algorithms we will cover later in this book. 

Simply put, the Fourier Transform is a beautiful application of complex number systems; however, it would rarely be used today if not for the ability to quickly perform the operation with Fast Fourier Transform, first introduced by the great Frederick Gauss in 1805 and later independently discovered by James Cooley and John Tukey in 1965 {{ "ct1965" | cite }}. 
Gauss (of course) already had too many things named after him and Cooley and Tukey both had cooler names, so the most common algorithm for FFT's today is known as the Cooley-Tukey algorithm.

### What is a Fourier Transform?

To an outsider, the Fourier Transform looks like a mathematical mess -- certainly a far cry from the heroic portal between two domains I have depicted it to be; however, like most things, it's not as bad as it initially appears to be. 
So, here it is in all it's glory!

$$F(\xi) = \int_{-\infty} ^\infty f(x) e^{-2 \pi i x \xi} dx$$

and

$$f(x) = \int_{-\infty} ^\infty F(\xi) e^{2 \pi i \xi x} d\xi$$

Where $$F(\xi)$$ represents a function in frequency space, $$\xi$$ represents a value in frequency space, $$f(x)$$ represents a function in real space, and $$x$$ represents a value in the real space. 
Note here that the only difference between the two exponential terms is a minus sign in the transformation to frequency space. 
As I mentioned, this is not intuitive syntax, so please allow me to explain a bit.

Firstly, **what does the Fourier Transform do?**

If we take a sum sinusoidal functions (like $$\sin(\omega t)$$ or $$\cos(\omega t)$$), we might find a complicated mess of waves between $$\pm 1$$.
Each constituent wave can be described by only one value: $$\omega$$.
So, instead of representing these curves as seen above, we could instead describe them as peaks in frequency space, as shown below. 

![Fourier Example](FT_example.png)

This is what the Fourier Transform does! 
After performing the transform, it is now much, much easier to understand precisely which frequencies are in our waveform, which is essential to most areas of signal processing.

Now, how does this relate to the transformations above? 
Well, the easiest way is to substitute in the Euler's formula:

$$e^{2 \pi i \theta} = \cos(2 \pi \theta) + i \sin(2 \pi \theta)$$

This clearly turns our function in frequency space into:

$$F(\xi) = \int_{-\infty} ^\infty f(x) (\cos(-2 \pi x \xi) + i \sin(-2 \pi x \xi))dx$$

and our function in real space into:

$$f(x) = \int_{-\infty} ^\infty F(\xi) (\cos(2 \pi \xi x) + i \sin(2 \pi \xi x))e^{2 \pi i \xi x} d\xi$$

Here, the $$\sin$$ and $$\cos$$ functions are clearly written in the formulas, so it looks much friendlier, right? 
This means that a point in real space is defined by the integral over all space of it's corresponding frequency function multiplied by sinusoidal oscillations. 

Truth be told, even after seeing this math, I still didn't understand Fourier Transforms. 
Truth be told, I didn't understand it fully until I discretized real and frequency space to create the Discrete Fourier Transform (DFT), which is the only way to implement Fourier Transforms in code.

### What is a Discrete Fourier Transform?

In principle, the Discrete Fourier Transform (DFT) is simply the Fourier transform with summations instead of integrals:

$$X_k = \sum_{n=0}^{N-1} x_n \cdot e^{-2 \pi k n / N}$$

and 

$$x_n = \frac{1}{N} \sum_{k=0}^{N-1} X_k \cdot e^{2 \pi k n / N}$$

Where $$X_n$$ and $$x_n$$ are sequences of $$N$$ numbers in frequency and real space, respectively. 
In principle, this is no easier to understand than the previous case! 
For some reason, though, putting code to this transformation really helped me figure out what was actually going on.

```
function DFT(x::Array{Float64})
    N = length(x)

    # We want two vectors here for real space (n) and frequency space (k)
    n = 0:N-1
    k = n'
    M = exp(-2im * pi *n *k / N)
    return x * M

end
```

In this function, we define `n` to be a set of integers from $$0 \rightarrow N-1$$ and arrange them to be a column. 
We then set `k` to be the same thing, but in a row. 
This means that when we multiply them together, we get a matrix, but not just any matrix! 
This matrix is the heart to the transformation itself!

```
M = [1.0+0.0im  1.0+0.0im           1.0+0.0im          1.0+0.0im; 
     1.0+0.0im  6.12323e-17-1.0im  -1.0-1.22465e-16im -1.83697e-16+1.0im; 
     1.0+0.0im -1.0-1.22465e-16im   1.0+2.44929e-16im -1.0-3.67394e-16im; 
     1.0+0.0im -1.83697e-16+1.0im  -1.0-3.67394e-16im  5.51091e-16-1.0im]
```

It was amazing to me when I saw the transform for what it truly was: an actual transformation matrix! 
That said, the Discrete Fourier Transform is slow -- primarily because matrix multiplication is slow, and as mentioned before, slow code is not particularly useful. 
So what was the trick that everyone used to go from a Discrete fourier Transform to a *Fast* Fourier Transform? 

Recursion!

### The Cooley-Tukey Algorithm

The problem with using a standard DFT is that it requires a large matrix multiplications and sums over all elements, which are prohibitively complex operations.
The Cooley-Tukey algorithm calculates the DFT directly with fewer summations and without matrix multiplications.
If necessary, DFT's can still be calculated directly at the early stages of the FFT calculation.
The trick to the Cooley-Tukey algorithm is recursion. 
In particular, we split the matrix we wish to perform the FFT on into two parts: one for all elements with even indices and another for all odd indices. 
We then proceed to split the array again and again until we have a manageable array size to perform a DFT (or similar FFT) on. 
We can also perform a similar re-ordering by using a bit reversal scheme, where we output each array index's integer value in binary and flip it to find the new location of that element.
With recursion, we can reduce the complexity to $$\sim \mathcal{O}(n \log n)$$, which is a feasible operation. 

In the end, the code looks like:
```
# Implementing the Cooley-Tukey Algorithm
function cooley_tukey(x)
    N = length(x)

    if(N%2 !=0)
        println("Must be a power of 2!")
        exit(0)
    end
    if(N <= 2)
        return DFT(x)
    else
        x_even = cooley_tukey(x[1:2:N])
        x_odd = cooley_tukey(x[2:2:N])
        n = 0:N-1
        half = div(N,2)
        factor = exp(-2im*pi*n/N)
        return vcat(x_even + factor[1:half] .* x_odd,
                     x_even + factor[half+1:N] .* x_odd) 
    end
    
end
```

As a side note, we are enforcing that the array must be a power of 2 for the operation to work. 
This is a limitation of the fact that we are using recursion and dividing the array in 2 every time; however, if your array is not a power of 2, you can simply pad the leftover space with 0's until your array is a power of 2.

The above method is a perfectly valid FFT; however, it is missing the pictorial heart and soul of the Cooley-Tukey algorithm: Butterfly Diagrams.

### Butterfly Diagrams
Butterfly Diagrams show where each element in the array goes before, during, and after the FFT. 
As mentioned, the FFT must perform a DFT. 
This means that even though we need to be careful about how we add elements together, we are still ultimately performing the following operation:

$$X_k = \sum_{n=0}^{N-1} x_n \cdot e^{-2 \pi k n / N}$$

However, after shuffling the initial array (by bit reversing or recursive subdivision), we perform the matrix multiplication of the $$e^{-2 \pi k n / N}$$ terms in pieces.
Basically, we split the array into a series of omega values:

$$\omega_N^k = e^{-2 \pi k / N}$$

And at each step, we use the appropriate term.
For example, imagine we need to perform an FFT of an array of only 2 elements. 
We can represent this addition with the following (radix-2) butterfly:

![Radix-2, positive W](radix-2screen_positive.jpg)

Here, the diagram means the following:

$$
b_0 = a_0 + \omega_2^0 a_1 \\

b_1 = a_0 + \omega_2^1 a_1
$$

However, it turns out that the second half of our array of $$\omega$$ values is always the negative of the first half, so $$\omega_2^0 = -\omega_2^1$$, so we can use the following butterfly diagram:

![Radix-2](radix-2screen.jpg)

With the following equations:

$$
b_0 = a_0 + \omega_2^0 a_1 \\

b_1 = a_0 - \omega_2^0 a_1
$$

By swapping out the second $$\omega$$ value in this way, we can save a good amount of space.
Now imagine we need to combine more elements.
In this case, we start with simple butterflies, as shown above, and then sum butterflies of butterflies.
For example, if we have 8 elements, this might look like this:

![Radix-8](radix-8screen.jpg)

Note that we can perform a DFT directly before using any butterflies, if we so desire, but we need to be careful with how we shuffle our array if that's the case.
In the code snippet provided in the previous section, the subdivision was performed in the same function as the concatenation, so the ordering was always correct; however, if we were to re-order with bit-reversal, this might not be the case.

For example, take a look at the ordering of FFT ([found on wikipedia](https://en.wikipedia.org/wiki/Butterfly_diagram)) that performs the DFT shortcut:

![Butterfly Diagram](butterfly_diagram.png)

Here, the ordering of the array was simply divided into even and odd elements once, but they did not recursively divide the arrays of even and odd elements again because they knew they would perform a DFT soon thereafter.

Ultimately, that's all I want to say about Fourier Transforms for now, but this chapter still needs a good amount of work!
I'll definitely come back to this at some point, so let me know what you liked and didn't like and we can go from there!
 
### Bibliography

{% references %} {% endreferences %}

### Example Code

To be clear, the example code this time will be complicated and requires the following functions:

* An FFT library (either in-built or something like FFTW)
* An approximation function to tell if two arrays are similar

As mentioned in the text, the Cooley-Tukey algorithm may be implemented either recursively or non-recursively, with the recursive method being much easier to implement.
I would ask that you implement either the recursive or non-recursive methods (or both, if you feel so inclined).
If the language you want to write your implementation in is already used, please append your code to the already existing codebase.
As before, pull requests are favoured.

Note: I implemented this in Julia because the code seems more straightforward in Julia; however, if you wish to write better Julia code or better code in your own language, please feel free to do so!
**I do not claim that this is the most efficient way to implement the Cooley-Tukey method, so if you have a better way to do it, feel free to implement it that way!**

#### Julia

```julia
#simple DFT function
function DFT(x)
    N = length(x)

    # We want two vectors here for real space (n) and frequency space (k)
    n = 0:N-1
    k = n'
    transform_matrix = exp.(-2im * pi *n *k / N)
    return transform_matrix*x

end

# Implementing the recursively Cooley-Tukey Algorithm
function cooley_tukey(x)
    N = length(x)

    if (N > 2)
        x_odd = cooley_tukey(x[1:2:N])
        x_even = cooley_tukey(x[2:2:N])
    else
        x_odd = x[1]
        x_even = x[2]
    end
    n = 0:N-1
    half = div(N,2)
    factor = exp.(-2im*pi*n/N)

    return vcat(x_odd + x_even .* factor[1:half],
                x_odd - x_even .* factor[1:half]) 

end

# Helper function for iterative Cooley Tukey
function bitreverse(a::Array)
    # First, we need to find the necessary number of bits
    digits = convert(Int,ceil(log2(length(a))))

    # Creating a range for the current element order
    # To be odified later
    indices = [i for i = 0:length(a)-1]

    # Creating a list of all the bits to flip later
    bit_indices = []
    for i = 1:length(indices)
        push!(bit_indices, bits(indices[i]))
    end

    # Now stripping the unnecessary numbers
    for i = 1:length(bit_indices)
        bit_indices[i] = bit_indices[i][end-digits:end]
    end

    # Flipping the bits
    for i =1:length(bit_indices)
        bit_indices[i] = reverse(bit_indices[i])
    end

    #replacing indices
    for i = 1:length(indices)
        indices[i] = 0
        for j = 1:digits
            indices[i] += 2^(j-1) * parse(string(bit_indices[i][end-j]))
        end
       indices[i] += 1
    end

    # replacing the elements as necessary
    b = [float(i) for i = 1:length(a)]
    for i = 1:length(indices)
        b[i] = a[indices[i]]
    end

    return b
end

# Iterative FFT method, requires butterfly() and bit_reverse()
function iterative_cooley_tukey(x)
    N = length(x)
    logN = convert(Int,ceil(log2(length(x))))

    # Number of butterflies every iteration, starting at logN / 2
    bnum = div(N,2)

    # The distance between butterflies
    stride = 0;

    # Function found above
    x = bitreverse(x)

    z = [Complex(x[i]) for i = 1:length(x)]
    for i = 1:logN
       stride = div(N, bnum)
       for j = 0:bnum-1
           start_index = j*stride + 1
           y = butterfly(z[start_index:start_index + stride - 1])
           for k = 1:length(y)
               z[start_index+k-1] = y[k]
           end
       end 

       # Halving butterfly number every timestep
       bnum = div(bnum,2)
    end

    return z
end

# Reads in an array and performs a butterfly operation over the array
function butterfly(x)
    N = length(x)
    half = div(N,2)
    n = [i for i = 0:N-1]
    half = div(N,2)
    factor = exp.(-2im*pi*n/N)

    y = [0 + 0.0im for i = 1:length(x)]

    for i = 1:half
        y[i] = x[i] + x[half+i]*factor[i]
        y[half+i] = x[i] - x[half+i]*factor[i]
    end

    return y
end

# Simple implementation of approximation function
function approx(x, y)
    val = true
    for i = 1:length(x)
        if (abs(x[i]) - abs(y[i]) > 1e-5)
            val = false
        end
    end
    println(val)
end

function main()
    x = rand(128)
    y = cooley_tukey(x)
    z = iterative_cooley_tukey(x)
    w = fft(x)
    approx(w,y)
    approx(w,z)
end

main()
```

#### Haskell

```hs
--- submitted by Jie
import Data.Complex
import Data.Array
import Data.Ratio
import qualified Data.Map as M

fft :: [Complex Double] -> [Complex Double]
fft x = let n = length x
            i = 0 :+ 1
            w = M.fromList [(k%n, exp ((-2)*pi*i*(fromIntegral k)/(fromIntegral n)) ) | k<-[0..n-1]]
            arr = fft' n w (listArray (0,n-1) x)
        in [arr!k | k<-[0..n-1]]
  where
  fft' 1 _ x = x
  fft' n w x = let n2 = div n 2
                   e = fft' n2 w (listArray (0, n2-1) [x!k | k<-[0,2..n-1]])
                   o = fft' n2 w (listArray (0, n2-1) [x!k | k<-[1,3..n-1]])
               in array (0, n-1) $ concat [[(k, e!k + o!k * w M.!(k%n)),
                                            (k + n2, e!k - o!k * w M.!(k%n))]
                                                               | k <- [0..n2-1]]

main = do
  print $ fft [0,1,2,3]

```

#### Scratch

Some rather impressive scratch code was submitted by Jie and can be found here: https://scratch.mit.edu/projects/37759604/#editor

#### C

```c
// written by Gathros.

#include <complex.h>
#include <math.h>

// These headers are for presentation not for the algorithm.
#include <stdlib.h>
#include <time.h>
#include <stdio.h>

#define PI 3.1415926535897932384626

void cooley_tukey(double complex *X, const size_t N){
        if(N >= 2){
                // Splits the array, so the top half are the odd elements 
		// and the bottom half are the even ones.
                double complex tmp [N/2];
                for(size_t i = 0; i < N/2; ++i){
                        tmp[i] = X[2*i + 1];
                        X[i] = X[2*i];
                }
                for(size_t i = 0; i < N/2; ++i){
                        X[i + N/2] = tmp[i];
                }

                // Recursion.
                cooley_tukey(X, N/2);
                cooley_tukey(X + N/2, N/2);

                // Combine.
                for(size_t i = 0; i < N/2; ++i){
                        X[i + N/2] = X[i] - cexp(-2.0*I*PI*i/N)*X[i + N/2];
                        X[i] -= (X[i + N/2]-X[i]);
                }
        }
}

void bit_reverse(double complex *X, size_t N){
        // Bit reverses the array X[] but only if the size of the array is less then 2^32.
                double complex temp;
        unsigned int b;

        for(unsigned int i = 0; i < N; ++i){
                b = i;
                b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
                b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
                b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
                b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
                b = ((b >> 16) | (b << 16)) >> (32 - (unsigned int) log2((double)N));
                if(b > i){
                        temp = X[b];
                        X[b] = X[i];
                        X[i] = temp;
                }
        }
}

void iterative_cooley_tukey(double complex *X, size_t N){
        int stride;
        double complex v,w;

        // Bit reverse the array.
        bit_reverse(X, N);

        // Preform the butterfly on the array.
        for(int i = 1; i <= log2((double)N); ++i){
                stride = pow(2, i);
                w = cexp(-2.0*I*PI/stride);
                for(size_t j = 0; j < N; j += stride){
                        v = 1.0;
                        for(size_t k = 0; k < stride/2; k++){
                                X[k + j + stride/2] = X[k + j] - v*X[k + j + stride/2];
                                X[k + j] -= (X[k + j + stride/2] - X[k + j]);
                                v *= w;
                        }
                }
        }
}

void approx(double complex *X, double complex *Y, size_t N){
        // This is to show that the arrays are approximate.
        for(size_t i = 0; i < N; ++i){
                printf("%f\n", cabs(X[i]) - cabs(Y[i]));
        }
}

int main(){
        // Initalizing the arrays for FFT.
        srand(time(NULL));
        const size_t N = 64;
        double complex x[N], y[N], z[N];
        for(size_t i = 0; i < N; ++i){
                x[i] = rand() / (double) RAND_MAX;
                y[i] = x[i];
                z[i] = x[i];
        }

        // Preform FFT.
        cooley_tukey(y, N);
        iterative_cooley_tukey(z, N);

        // Check if the different methods are approximate.
        approx(y, z, N);

        return 0;
}

```

#### C++

```c++
// written by Gathros, modernized by Nicole Mazzuca.

#include <complex>
#include <vector>
#include <array>
#include <cstdint>

// These headers are for presentation not for the algorithm.
#include <random>
#include <iostream>
#include <iomanip>

using c64 = std::complex<double>;
template <typename T>
constexpr T pi() {
  return 3.14159265358979323846264338327950288419716;
}

template <typename Iter, typename Iter_end>
void cooley_tukey(Iter start, Iter_end end) {
  auto size = end - start;
  if (size >= 2) {
    // Splits the array, so the top half are the odd elements and the bottom are the even ones.
    auto temp = std::vector<c64>(size / 2);
    for (std::size_t i = 0; i < size / 2; ++i) {
      temp[i] = start[i * 2 + 1];
      start[i] = start[i * 2];
    }
    for (std::size_t i = 0; i < size / 2; ++i) {
      start[i + size / 2] = temp[i];
    }

    // Recursion.
    cooley_tukey(start, start + size / 2);
    cooley_tukey(start + size / 2, end);

    // Combine.
    for (std::size_t k = 0; k < size / 2; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double>() * k / size));
      start[k + size / 2] = start[k] - w * start[k + size / 2];
      start[k] -= (start[k + size / 2] - start[k]);
    }
  }
}


template <typename Iter, typename Iter_end>
void bit_reverse(Iter start, Iter_end end) {
  // Bit reverses the array X[] but only if the size of the array is less then 2^32.
  auto size = end - start;

  for (std::uint32_t i = 0; i < size; ++i) {
    auto b = i;
    b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
    b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
    b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
    b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
    b = ((b >> 16) | (b << 16)) >> (32 - std::uint32_t(log2(size)));
    if (b > i) {
      std::swap(start[b], start[i]);
    }
  }
}

template <typename Iter, typename Iter_end>
void iterative_cooley_tukey(Iter start, Iter_end end) {
  // Bit reverse the array.
  bit_reverse(start, end);

  //Preform the butterfly on the array.
  auto size = end - start;
  for (std::size_t stride = 2; stride <= size; stride *= 2) {
    auto w = exp(c64(0, -2.0 * pi<double>() / stride));
    for (std::size_t j = 0; j < size; j += stride) {
      auto v = c64(1.0);
      for (std::size_t k = 0; k < stride / 2; k++) {
        start[k + j + stride / 2] =
          start[k + j] - v * start[k + j + stride / 2];
        start[k + j] -= (start[k + j + stride / 2] - start[k + j]);
        v *= w;
      }
    }
  }
}

int main() {
  // Initalizing the FFT inputs.
  auto random_number_generator = std::mt19937_64();
  auto generate_random_double = [&]() {
    auto rn = random_number_generator();
    return double(rn) / double(UINT64_MAX);
  };

  std::array<c64, 64> initial;

  for (auto& el : initial) {
    el = generate_random_double();
  }

  auto recursive = initial;
  auto iterative = initial;

  // Preform an FFT on the arrays.
  cooley_tukey(recursive.begin(), recursive.end());
  iterative_cooley_tukey(iterative.begin(), iterative.end());

  // Check if the arrays are approximate.
  std::cout
    << std::right
    << std::setw(16) << "idx"
    << std::setw(16) << "rec"
    << std::setw(16) << "it"
    << std::setw(16) << "subtracted"
    << '\n';
  for (int i = 0; i < initial.size(); ++i) {
    auto rec = recursive[i];
    auto it = iterative[i];
    std::cout
      << std::setw(16) << i
      << std::setw(16) << std::abs(rec)
      << std::setw(16) << std::abs(it)
      << std::setw(16) << (std::abs(rec) - std::abs(it))
      << '\n';
  }
}

```

#### Python
```python
# Submitted by Gathros

from random import random
from cmath import exp, pi
from math import log2

def cooley_tukey(X):
        N = len(X)
        if N <= 1:
                return X
        even = cooley_tukey(X[0::2])
        odd =  cooley_tukey(X[1::2])

        temp = [i for i in range(N)]
        for k in range(N//2):
                temp[k] = even[k] + exp(-2j*pi*k/N) * odd[k]
                temp[k+N//2] = even[k] - exp(-2j*pi*k/N)*odd[k]
        return temp

def bitReverse(X):
        N = len(X)
        temp = [i for i in range(N)]
        for k in range(N):
                b =  sum(1<<(int(log2(N))-1-i) for i in range(int(log2(N))) if k>>i&1)
                temp[k] = X[b]
                temp[b] = X[k]
        return temp

def iterative_cooley_tukey(X):
        N = len(X)

        X = bitReverse(X)

        for i in range(1, int(log2(N)) + 1):
                stride = 2**i
                w = exp(-2j*pi/stride)
                for j in range(0, N, stride):
                        v = 1
                        for k in range(stride//2):
                                X[k + j + stride//2] = X[k + j] - v*X[k + j + stride//2];
                                X[k + j] -= (X[k + j + stride//2] - X[k + j]);
                                v *= w;
        return X

X = []

for i in range(64):
        X.append(random())

Y = cooley_tukey(X)
Z = iterative_cooley_tukey(X)

print(all(abs([Y[i] - Z[i] for i in range(64)][j]) < 1 for j in range(64)))

```
