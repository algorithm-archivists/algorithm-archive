// Submitted by Christopher Milan (christopherm99)

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
    a := []int{1, 3, 4, 2}
    bogoSort(&a)
    fmt.Println(a)
}
