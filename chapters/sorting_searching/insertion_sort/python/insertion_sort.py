def insertion_sort(array):
	
	array_length = len(array)
	# loop through array[1:n], array[0] is already sorted
	for j in range(1, array_length):
		current_element = array[j]

		# Place the j-th element to the correct position in the sub array array[0...j]
		# Keeping array[0...j] sorted
		i = j - 1
		while((i >= 0) and (array[i] > current_element)):
			array[i + 1] = array[i]
			i -= 1

		array[i + 1] = current_element

	return array


if __name__ == '__main__':
	array = [10, 1, 3, 4, 7, 2, 5, 9, 6, 8]
	sorted_array = insertion_sort(array)

	print("This is the array of sorting: " + str(sorted_array))