verlet <- function(pos, acc, dt) {
  prev_pos <- pos
  time <- 0
  
  while (pos > 0) {
    time <- time + dt
    next_pos <- pos * 2 - prev_pos + acc * dt * dt
    prev_pos <- pos
    pos <- next_pos
  }
  return(time)
}

stormer_velvet <- function(pos, acc, dt) {
  prev_pos <- pos
  time <- 0
  vel <- 0
  
  while (pos > 0) {
    time <- time + dt
    next_pos <- pos * 2 - prev_pos + acc * dt * dt
    prev_pos <- pos
    pos <- next_pos
    vel <- vel + acc * dt
  }
  
  #R functions can't return multiple objects
  #Thus, we will put them into a list, and return the list instead
  values <- list(time, vel)
  return(values)
}

velocity_verlet <- function(pos, acc, dt) {
  time <- 0
  vel <- 0
  
  while (pos > 0) {
    time <- time + dt
    pos <- pos + (vel * dt + 0.5 * acc * dt * dt)
    vel <- vel + acc * dt
  }
  
  #Same as before
  values <- list(time, vel)
  return(values)
}

main_function <- function() {
  time <- verlet(5, -10, 0.01)
  print("Verlet")
  #sprintf() helps us format our values but it returns an object (character vector)
  #We use cat() in order to output that object. print() would work fine too
  cat(sprintf("Time: %.10f\n", time))
  
  stormer_verlet_list <- stormer_velvet(5, -10, 0.01)
  time <- stormer_verlet_list[1]
  vel <- stormer_verlet_list[2]
  print("Stormer verlet")
  cat(sprintf("Time: %.10f, Velocity: %.10f\n", time, vel))
  
  velocity_verlet_list <- velocity_verlet(5, -10, 0.01)
  time <- velocity_verlet_list[1]
  vel <- velocity_verlet_list[2]
  print("Velocity verlet")
  cat(sprintf("Time: %.10f, Velocity: %.10f\n", time, vel))
}

main_function()
