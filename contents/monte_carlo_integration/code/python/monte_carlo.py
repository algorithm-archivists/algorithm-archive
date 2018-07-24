# submitted by hybrideagle
from random import random
from math import pi


def in_circle(x_pos, y_pos):
    radius = 1
    # Compute euclidian distance from origin
    return (x_pos * x_pos + y_pos * y_pos) < (radius * radius)


def monte_carlo(n):
    """
    Computes PI using the monte carlo method using `n` points 
    """
    pi_count = 0

    for i in range(n):
        x = random()
        y = random()
        if in_circle(x, y):
            pi_count += 1

    # This is using a quarter of the unit sphere in a 1x1 box.
    # The formula is pi = (boxLength^2 / radius^2) * (piCount / n), but we
    # are only using the upper quadrant and the unit circle, so we can use
    # 4*piCount/n instead
    # piEstimate = 4*piCount/n
    pi_estimate = 4 * pi_count / n
    print('Pi is {0:} ({1:.4f}% error)'.format(
        pi_estimate, (pi - pi_estimate) / pi * 100))


# If this file was run directly
if __name__ == "__main__":
    monte_carlo(100000)
