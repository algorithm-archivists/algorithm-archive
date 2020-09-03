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

	# Since the ratio this calculates is pi/4 ((pi*r^2)/4) / r^2)
	# We need to multiply the number of points by 4	to get pi
	pi_estimate = 4 * in_circle_count / (n_samples)

	return pi_estimate

if __name__ == '__main__':

	pi_estimate = monte_carlo(100000)
	percent_error = 100*abs(math.pi - pi_estimate)/math.pi

	print("The estimate of pi is: {:.3f}".format(pi_estimate))
	print("The percent error is: {:.3f}".format(percent_error))

