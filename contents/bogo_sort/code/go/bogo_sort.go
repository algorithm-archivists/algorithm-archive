// Submitted by Christopher Milan (christopherm99), edited by Prajwal Krishna

package main

import (
	"fmt"
	"math/rand"
	"time"
)

func shuffle(a *[]int) {
	for i := len(*a) - 1; i > 0; i-- {
		j := rand.Intn(i + 1)
		(*a)[i], (*a)[j] = (*a)[j], (*a)[i]
	}
}

func isSorted(a []int) bool {
	for i := 0; i < len(a)-1; i++ {
		if a[i+1] < a[i] {
			return false
		}
	}
	return true
}

func bogoSort(a *[]int) {
	for !isSorted(*a) {
		shuffle(a)
	}
}

func main() {
	rand.Seed(time.Now().UnixNano())
	a := []int{1, 3654, 78, 654, -234, -12, 4, 3, -6, -100}
	fmt.Println("Unsorted array:")
	fmt.Println(a)
	fmt.Println()
	bogoSort(&a)
	fmt.Println("Sorted array:")
	fmt.Println(a)
}
