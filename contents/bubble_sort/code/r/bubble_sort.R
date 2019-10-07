bubble_sort <- function(a) {
  for( i in 1:length(a) ) {
    for( j in 1:( length(a) - 1) ){
      if( a[j] > a[j + 1] ){
        tmp <- a[j]
        a[j] <- a[j + 1]
        a[j + 1] <- tmp
      }
    }
  }
  return(a)
}

unsorted <- c(4, 2, -3, 1)

print("unsorted vector")
print(unsorted)

print("sorted vector")
print(bubble_sort(unsorted)
