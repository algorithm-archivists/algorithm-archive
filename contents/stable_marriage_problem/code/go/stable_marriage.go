//Coded by Prajwal Krishna Maitin (pk1210)
package main

import "fmt"
import "time"
import "math/rand"

type person struct {
	ID      int
	prefers []int
	partner *person
	index   int
}

func suffle(arr []int, size int) []int {
	seed := rand.NewSource(time.Now().UnixNano())
	generator := rand.New(seed)

	for i := 0; i < size; i++ {
		j := generator.Intn(i + 1)
		temp := arr[i]
		arr[i] = arr[j]
		arr[j] = temp
	}
	return arr
}

func createGroup(size int) []person {
	group := make([]person, size)
	for i := 0; i < size; i++ {
		group[i].ID = i
		group[i].partner = nil
		group[i].index = 0
		arr := make([]int, size)
		for j := 0; j < size; j++ {
			arr[j] = j
		}
		group[i].prefers = suffle(arr, size)
	}
	return group
}

func perferredPartner(man *person, woman *person, size int) bool {
	//Function checks if man is liked more than woman's current partner
	partner := woman.partner.ID
	bachelor := man.ID
	for i := 0; i < size; i++ {
		if woman.prefers[i] == partner {
			return false
		} else if woman.prefers[i] == bachelor {
			return true
		}
	}
	return false
}
func stageMarriage(men []person, women []person, size int) {
	bachelors := make([]*person, size)
	bachelorsSize := size
	for i := 0; i < size; i++ {
		bachelors[i] = &men[i]
	}
	for bachelorsSize > 0 {
		man := bachelors[bachelorsSize-1]
		//Selecting preferred women from current man
		woman := &women[man.prefers[man.index]]

		if woman.partner == nil {
			//If woman does not have any partner assign them current bachelors
			woman.partner = man
			man.partner = woman
			bachelors[bachelorsSize-1] = nil
			bachelorsSize--
		} else if perferredPartner(man, woman, size) {
			//If woman has a partner check if she likes current man more
			ex := woman.partner
			ex.partner = nil
			woman.partner = man
			man.partner = woman
			bachelors[bachelorsSize-1] = ex
		} else {
			//Tell man to look for someother woman
			man.index++
		}
	}
}

func main() {
	size := 5
	men := createGroup(size)
	women := createGroup(size)

	fmt.Println("Men Preference List")
	for i := 0; i < size; i++ {
		fmt.Println("Preference of man ", i, men[i].prefers)
	}
	fmt.Println("\nWomen Preference List")
	for i := 0; i < size; i++ {
		fmt.Println("Preference of woman ", i, men[i].prefers)
	}

	stageMarriage(men, women, size)

	fmt.Println("\nStable Marriage")
	for i := 0; i < size; i++ {
		fmt.Println("Partner of woman ", i, "is man", women[i].partner.ID)
	}
}
