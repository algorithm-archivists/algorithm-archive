fn bubblesort(arr mut []int) {
	for i := 0; i < arr.len-1; i++ {
		for j := 0; j < arr.len-i-1; j++ {
			if arr[j] > arr[j+1] {
				tmp := arr[j]
				arr[j] = arr[j+1]
				arr[j+1] = tmp
			}
		}
	}
}

fn main() {
	mut arr := [1, 45, 756, 4569, 56, 3, 8, 5, -10, -4]
	println('Array unsorted:')
	println(arr)
	bubblesort(mut arr)
	println('Array sorted:')
	println(arr)
}