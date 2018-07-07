
in_circle <- function(x, y, radius = 1){
        # Return True if the point is in the circle and False otherwise.
        return((x*x + y*y) < radius*radius)
}

monte_carlo <- function(n_samples, radius = 1){
# Return the estimate of pi using the monte carlo algorithm.
        
        # Sample x, y from the uniform distribution
        x <- runif(n_samples, 0, radius)
        y <- runif(n_samples, 0, radius)

        # Count the number of points inside the circle
        in_circle_count <- sum(in_circle(x, y, radius))

        # Since we've generated points in upper left quadrant ([0,radius], [0,])
        # We need to multiply the number of points by 4	
        pi_estimate <- 4 * in_circle_count / n_samples

        return(pi_estimate)
}

pi_estimate <- monte_carlo(10000000)
percent_error <- abs(pi - pi_estimate)/pi

print(paste("The estimate of pi is: ", formatC(pi_estimate)))
print(paste("The percent error is:: ", formatC(percent_error)))
