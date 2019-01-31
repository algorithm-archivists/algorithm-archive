fwd_euler <- function(time_step, n) {
  # the integer() function creates a sequence of zeros of integer type.  The parameter that is passed
  #indicates the length/size of the sequence.
  result <- integer(n)
  #setting the initial condition
  result[0] <- 1
  for (i in 2:n) {
    result[i] <- result[i-1] - 3 * result[i-1] * time_step
  }
  return(result)
}

check <- function(result, threshold, time_step) {
  approx <- TRUE
  n <- length(result)
  for (i in 1:n) {
    solution <- exp(-3 * i * time_step)
    if (abs(result[i] - solution  > threshold)) {
      print(result[i])
      print(solution)
      approx <- FALSE
    }
  }
  return(approx)
}

main_function <- function() {
  time_step <- 0.01
  n <- 100
  threshold <- 0.01

  res <- fwd_euler(time_step, n)
  approx <- check(res, threshold, time_step)

  if (approx) print("All values within threshold") else print("Value(s) not in threshold")
}

main_function()
