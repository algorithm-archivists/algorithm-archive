// Submitted by Chinmaya Mahesh (chin123)

package main

import "fmt"

func abs(a int) int {
	if a < 0 {
		a = -a
	}
	return a
}

func euclidMod(a, b int) int {
	a = abs(a)
	b = abs(b)

	for b != 0 {
		a, b = b, a%b
	}

	return a
}

func euclidSub(a, b int) int {
	a = abs(a)
	b = abs(b)

	for a != b {
		if a > b {
			a -= b
		} else {
			b -= a
		}
	}

	return a
}

func main() {
	check1 := euclidMod(64*67, 64*81)
	check2 := euclidSub(128*12, 128*77)

	fmt.Println(check1)
	fmt.Println(check2)
}
