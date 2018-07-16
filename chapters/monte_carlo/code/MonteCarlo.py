import math
import random


def in_circle(x, y, rad):
    if x**2 + y**2 < rad**2:
        return True
    else:
        return False


def monte_carlo_integration(sample, rad):
    count = 0
    for i in range(sample):
        x = random.random()*2 - 1
        y = random.random()*2 - 1
        if in_circle(x, y, rad):
            count += 1

    estimate = (4*count)/(sample*rad*rad)
    print("The Estimate of Pi is: %s" % estimate)
    print("The error is: %s" % (math.pi - estimate))


monte_carlo_integration(1000000, 1)
