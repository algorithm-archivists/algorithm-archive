// Submitted by Christopher Milan (christopherm99)

package main

import (
	"fmt"
	"math/rand"
	"time"
)

func shuffle(a []int) []int {
	rand.Seed(time.Now().UnixNano())
	for i := len(a) - 1; i > 0; i-- {
		j := rand.Intn(i + 1)
		a[i], a[j] = a[j], a[i]
	}
	return a
}

func is_sorted(a []int) bool {
	for i := 0; i < len(a)-1; i++ {
		if a[i+1] < a[i] {
			return false
		}
	}
	return true
}

func bogo_sort(a *[]int) {
	for !is_sorted(*a) {
		*a = shuffle(*a)
	}
}

func main() {
	a := []int{1, 3, 4, 2}
	bogo_sort(&a)
	fmt.Println(a)
}
