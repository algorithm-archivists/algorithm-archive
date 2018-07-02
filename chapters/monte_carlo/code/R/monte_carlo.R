
in_circle <- function(x, y){
        # Return True if the point is in the circle and False otherwise.
        radius = 1.0
        return((x*x + y*y) < radius*radius)
}

monte_carlo <- function(n_samples){
# Return the estimate of pi using the monte carlo algorithm.
        radius = 1.0
        
        # Sample x, y from the uniform distribution
        x <- runif(n_samples, 0, 1)
        y <- runif(n_samples, 0, 1)

        # Count the number of points inside the circle
        in_circle_count <- sum(in_circle(x, y))

        # Since we've generated points in upper left quadrant ([0,1], [0,1])
        # We need to multiply the number of points by 4	
        pi_estimate <- 4 * in_circle_count / (n_samples * radius * radius)

        return(pi_estimate)
}

pi_estimate <- monte_carlo(100000)
percent_error <- abs(pi - pi_estimate)/pi

print(paste("The estimate of pi is: ", formatC(pi_estimate)))
print(paste("The percent error is:: ", formatC(percent_error)))
