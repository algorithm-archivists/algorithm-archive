import math
import random


def in_circle(x, y):
	radius = 1.0
	"""Return True if the point is in the circle and False otherwise."""
	return (x*x + y*y) < radius*radius

def monte_carlo(n_samples):
	"""Return the estimate of pi using the monte carlo algorithm."""
	in_circle_count = 0
	for i in range(n_samples):
		
		# Sample x, y from the uniform distribution
		x = random.uniform(0,1)
		y = random.uniform(0,1)
		
		# Count the number of points inside the circle
		if(in_circle(x, y, radius)):
			in_circle_count += 1

	# Since we've generated points on in upper left quadrant ([0,1], [0,1])
	# We need to mulply the number of points by 4	
	pi_estimate = 4 * in_circle_count / (n_samples * radius * radius)

	return pi_estimate

if __name__ == '__main__':

	pi_estimate = monte_carlo(100000)
	percent_error = abs(math.pi - pi_estimate)/math.pi

	print("The estimate of pi is: %.3f" % pi_estimate)
	print("The percent error is: %.3f" %  percent_error)

