bubble_sort <- function(array) {
  len_array <- length(array)
  for (i in 1:(len_array-1)) {
    for (j in 1:(len_array-i)) {
      if (array[j] > array[j+1]) {
        #'temp' will hold the current array[j] value because it will be changed
        temp <- array[j]
        array[j] <- array[j+1]
        array[j+1] <- temp
      }
    }
  }
  return(array)
}

main_function <- function() {
  #the sample function creates a vector/1d array with random integers
  #100 states the right endpoint of the closed interval, [1, 100]
  #15 states the amount of integers we want in our vector/1d array
  random_array <- sample(100, 15)
  print("Before sorting:")
  print(random_array)
  print("After sorting:")
  print(bubble_sort(random_array))
}

main_function()


