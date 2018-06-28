#example submitted by Jonas Vander Vennet, Python 3.5.2

import random, math

# function to determine whether an x, y point is inside a circle with given radius
def in_circle(x, y, radius):
    return x**2 + y**2 < radius**2

# function to integrate a circle with given radius via monte carlo integration
def monte_carlo(n, radius):
    count = 0
    for i in range(n):
        count += in_circle(random.uniform(0,radius),random.uniform(0,radius),radius)
    return 4*(count/n)*radius**2

#estimate pi by integrating the unit circle
estimated_pi = monte_carlo(10**6, 1)

print("percent error: {:%}".format(abs(estimated_pi-math.pi)/math.pi))
print("Estimation for pi: {:f}".format(estimated_pi))