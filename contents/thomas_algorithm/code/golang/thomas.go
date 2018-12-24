package main

import "fmt"

func thomas(a, b, c, d []float64) []float64 {
	c[0] = c[0] / b[0]
	d[0] = d[0] / b[0]

	for i := 1; i < len(d); i++ {
		scale := 1. / (b[i] - c[i-1]*a[i])
		c[i] *= scale
		d[i] = (d[i] - a[i]*d[i-1]) * scale
	}

	for i := len(d) - 2; i >= 0; i-- {
		d[i] -= c[i] * d[i+1]
	}

	return d
}

func main() {
	a := []float64{0., 2., 3.}
	b := []float64{1., 3., 6.}
	c := []float64{4., 5., 0.}
	d := []float64{7., 5., 3.}

	fmt.Println("The system,")
	fmt.Println("[1.0  4.0  0.0][x] = [7.0]")
	fmt.Println("[2.0  3.0  5.0][y] = [5.0]")
	fmt.Println("[0.0  3.0  6.0][z] = [3.0]")
	fmt.Println("has the solution:")
	solve := thomas(a, b, c, d)
	for _, i := range solve {
		fmt.Printf("[%f]\n", i)
	}
}
