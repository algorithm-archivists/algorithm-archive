import random, math

# function to determine whether an x, y point is inside a circle with given radius
def in_circle(x, y, radius):
    return x**2 + y**2 < radius**2

# function to integrate a circle with given radius via monte carlo integration
def monte_carlo(n, radius):
    count = 0
    for i in range(n):
        count += in_circle(random.uniform(0,radius),random.uniform(0,radius),radius)
    estimated_area = 4*(count/n)*radius**2
    exact_area = math.pi*radius**2
    print("percent error: %.10f%%"%(abs(estimated_area-exact_area)/exact_area*100))
    return estimated_area

#estimate pi by integrating the unit circle
estimated_area = monte_carlo(10**6, 1)
print("Estimation for pi: %.10f"%estimated_area)