euclid_sub <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while(a != b) {
    if (a > b) a <- a - b else b <- b - a 
  }
  return(a)
}

euclid_mod <- function(a, b) {
  while (b != 0) {
    #'temp' will hold the current value of b since it will be changed
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}

main_function <- function() {
  print("Euclidian mod:")
  print(euclid_mod(64*67, 64*81))
  print("Euclidian sub")
  print(euclid_sub(128*12, 128*77))
}

main_function()
