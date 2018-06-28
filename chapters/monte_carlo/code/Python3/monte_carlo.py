import random
import math


def in_circle(x, y, r):
    """
    return True if (x,y) is inside a circle with radius r
    :param x: int or float, x position of a point
    :param y: int or float, y position of a point
    :param r: int or float, radius of circle
    :return: True or false
    """
    return x ** 2 + y ** 2 < r ** 2


def monte_carlo(n, r):
    """
    calculate pi
    :param n: int, number of points
    :param r: radius of circle
    :return: estimate of pi,
    """
    points_in_circle = 0
    for _dummy in range(n):
        x = random.random()
        y = random.random()
        if in_circle(x, y, r):
            points_in_circle += 1

    pi_estimate = 4.0 * points_in_circle / (n * r ** 2)
    pi_erro = 100*(math.pi - pi_estimate)/math.pi
    return pi_estimate, pi_erro

print(monte_carlo(10000000, 0.5))
