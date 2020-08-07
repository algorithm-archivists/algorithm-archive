bogo_sort <- function(a) {
  while(is.unsorted(a)) {
    a <- sample(a)
  }
  return(a)
}

test <- c(20, -3, 50, 1, -6, 59)

print("unsorted list")
print(test)

print("sorted list")
print(bogo_sort(test))

