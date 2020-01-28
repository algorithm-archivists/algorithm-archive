package main

import (
	"fmt"
	"github.com/runningwild/go-fftw/fftw"
	"math"
	"math/cmplx"
	"math/rand"
	"time"
)

func printSlice(x []complex128) {
	fmt.Printf("len=%d cap=%d slice=%v\n\n", len(x), cap(x), x)
}

func fft(x []complex128) {
	y := fftw.NewArray(len(x))
	for i := 0; i < len(x); i++ {
		y.Set(i, x[i])
	}

	forward := fftw.NewPlan(y, y, fftw.Forward, fftw.Estimate)
	forward.Execute()
	forward.Destroy()

	for i := range x {
		x[i] = y.At(i) / cmplx.Sqrt(complex(float64(len(x)), 0))
	}
}

func dft(x []complex128) {
	tmp := make([]complex128, len(x))
	for i := 0; i < len(x); i++ {
		tmp[i] = 0
		for j := 0; j < len(x); j++ {
			tmp[i] += x[j] * cmplx.Exp(complex(0, -2*math.Pi*float64(j*i)/float64(len(x))))
		}
	}

	copy(x, tmp)
}

func cooleyTukey(x []complex128) {
	if len(x) <= 1 {
		return
	}

	tmp := make([]complex128, len(x)/2)
	for i := 0; i < len(x)/2; i++ {
		tmp[i] = x[2*i+1]
		x[i] = x[2*i]
	}

	copy(x[len(x)/2:], tmp)

	cooleyTukey(x[:len(x)/2])
	cooleyTukey(x[len(x)/2:])

	for i := 0; i < len(x)/2; i++ {
		x[i+len(x)/2] = x[i] -
			cmplx.Exp(complex(0, -2*math.Pi*float64(i)/float64(len(x))))*x[i+len(x)/2]
		x[i] -= x[i+len(x)/2] - x[i]
	}
}

func bitReverse(x []complex128) {
	for i := 0; i < len(x); i++ {
		n, a := i, i
		count := int(math.Log2(float64(len(x)))) - 1

		n >>= 1
		for n > 0 {
			a = (a << 1) | (n & 1)
			count--
			n >>= 1
		}
		n = (a << count) & ((1 << int(math.Log2(float64(len(x))))) - 1)

		if n > i {
			x[i], x[n] = x[n], x[i]
		}
	}
}

func iterativeCooleyTukey(x []complex128) {
	bitReverse(x)

	for i := 1; i <= int(math.Log2(float64(len(x)))); i++ {
		stride := int(math.Pow(2, float64(i)))
		w := cmplx.Exp(complex(0, -2*math.Pi/float64(stride)))
		for j := 0; j < len(x); j += stride {
			v := complex(1, 0)
			for k := 0; k < stride/2; k++ {
				x[k+j+stride/2] = x[k+j] - v*x[k+j+stride/2]
				x[k+j] -= (x[k+j+stride/2] - x[k+j])
				v *= w
			}
		}
	}
}

func approx(x, y []complex128) {
	for i := 0; i < len(x); i++ {
		if cmplx.Abs(x[i])-cmplx.Abs(y[i]) > 1e-5 {
			fmt.Println("This is not approximate.")
			return
		}
	}
	fmt.Println("This is approximate.")
}

func main() {
	rand.Seed(time.Now().UnixNano())
	x := make([]complex128, 64)
	y := make([]complex128, 64)
	z := make([]complex128, 64)
	w := make([]complex128, 64)
	for i := 0; i < len(x); i++ {
		x[i] = complex(rand.Float64(), 0)
		y[i] = x[i]
		z[i] = x[i]
		w[i] = x[i]
	}

	fft(w)
	dft(x)
	cooleyTukey(y)
	iterativeCooleyTukey(z)

	printSlice(x)
	printSlice(y)
	printSlice(z)

	approx(w, x)
	approx(w, y)
	approx(w, z)
}
