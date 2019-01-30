thomas_algorithm <- function(a, b, c, d) {
  
  #set the initial elements
  c[0] <- c[0] / b[0]
  d[0] <- d[0] / b[0]
  
  n <- length(d)
  
  for (i in 2:n) {
    #scale factor for c and d
    scale <- 1 / (b[i] - c[i-1] * a[i])
    c[i] <- c[i] * scale
    d[i] <- (d[i] - a[i] * d[i-1]) * scale
  }
  
  #back substitution
  for (i in (n-1):1) {
    d[i] <- d[i] - (c[i] * d[i+1])
  }
  
  return(d)
}

main <- function() {
  #the c() function combines its arguments in order to form a vector
  a <- c(0, 2, 3)
  b <- c(1, 3, 6)
  c <- c(4, 5, 0)
  d <- c(7, 5, 3)
  
  print(thomas_algorithm(a, b, c, d))
}

main()