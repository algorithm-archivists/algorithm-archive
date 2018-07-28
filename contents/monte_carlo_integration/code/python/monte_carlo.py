import math
import random


def in_circle(x, y, radius = 1):
	"""Return True if the point is in the circle and False otherwise."""
	return (x*x + y*y) < radius*radius

def monte_carlo(n_samples, radius = 1):
	"""Return the estimate of pi using the monte carlo algorithm."""
	in_circle_count = 0
	for i in range(n_samples):
		
		# Sample x, y from the uniform distribution
		x = random.uniform(0, radius)
		y = random.uniform(0, radius)
		
		# Count the number of points inside the circle
		if(in_circle(x, y, radius)):
			in_circle_count += 1

	# Since we've generated points in upper left quadrant ([0,radius], [0, radius])
	# We need to multiply the number of points by 4	
	pi_estimate = 4 * in_circle_count / (n_samples)

	return pi_estimate

if __name__ == '__main__':

	pi_estimate = monte_carlo(100000)
	percent_error = 100*abs(math.pi - pi_estimate)/math.pi

	print("The estimate of pi is: {:.3f}".format(pi_estimate))
	print("The percent error is: {:.3f}".format(percent_error))

