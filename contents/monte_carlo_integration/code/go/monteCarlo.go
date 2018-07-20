// Submitted by Chinmaya Mahesh (chin123)

package main

import (
	"fmt"
	"math"
	"math/rand"
	"time"
)

func inCircle(x, y float64) bool {
	return x*x+y*y < 1.0 // the radius of an unit circle is 1.0
}

func monteCarlo(samples int) {
	count := 0
	s := rand.NewSource(time.Now().UnixNano())
	r := rand.New(s)

	for i := 0; i < samples; i++ {
		x, y := r.Float64(), r.Float64()

		if inCircle(x, y) {
			count += 1
		}
	}
	
	estimate := 4.0 * float64(count) / float64(samples)

	fmt.Println("The estimate of pi is", estimate)
	fmt.Printf("Which has an error of %f%%\n", 100*math.Abs(math.Pi-estimate)/math.Pi)
}

func main() {
	monteCarlo(10000000)
}
