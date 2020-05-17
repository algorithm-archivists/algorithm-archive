bubble_sort <- function(arr) {
  for (i in seq_along(arr)) {
    for (j in seq_along(arr[-1])) {
      if (arr[j] > arr[j + 1]) {
        tmp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- tmp
      }
    }
  }
  return(arr)
}

x <- c(20, -3, 50, 1, -6, 59)
print("Before sorting:")
print(x)

x <- bubble_sort(x)
print("After sorting:")
print(x)
