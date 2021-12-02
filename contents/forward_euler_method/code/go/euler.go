package main

import (
	"fmt"
	"math"
)

func forwardEuler(timeStep float64, n int) []float64 {
	result := make([]float64, n)
	result[0] = 1
	for x := 1; x < n; x++ {
		result[x] = result[x-1] - 3*result[x-1]*timeStep
	}
	return result
}

func check(result []float64, threshold, timeStep float64) bool {
	approx := true
	for x := 0.; int(x) < len(result); x++ {
		solution := math.Exp(-3. * x * timeStep)
		if math.Abs(result[int(x)]-solution) > threshold {
			fmt.Println(result[int(x)], solution)
			approx = false
		}
	}
	return approx
}

func main() {
	timeStep, threshold := .01, .01
	n := 100

	result := forwardEuler(timeStep, n)
	if check(result, threshold, timeStep) {
		fmt.Println("All values within threshold")
	} else {
		fmt.Println("Value(s) not within threshold")
	}
}
