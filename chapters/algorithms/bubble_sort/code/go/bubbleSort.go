// Submitted by Chinmaya Mahesh (chin123)

package main

import "fmt"

func bubbleSort(array []int) {
	n := len(array)
	for i := 0; i < n-1; i++ {
		swapped := false
		for j := 0; j < n-i-1; j++ {
			if array[j] > array[j+1] {
				array[j], array[j+1] = array[j+1], array[j]
				swapped = true
			}
		}
		if !swapped {
			break
		}
	}
}

func main() {
	array := [10]int{1, 45, 756, 4569, 56, 3, 8, 5, -10, -4}
	fmt.Println("Unsorted array:", array)

	bubbleSort(array[:])

	fmt.Println("Sorted array:", array)
}
