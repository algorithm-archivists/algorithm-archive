# submitted by hybrideagle
from random import random
from math import pi


def inCircle(xPos, yPos):
    radius = 1
    # Compute euclidian distance from origin
    return (xPos * xPos + yPos * yPos) < (radius * radius)


def monteCarlo(n):
    """
    Computes PI using the monte carlo method using `n` points 
    """
    piCount = 0

    for i in range(n):
        pointX = random()
        pointY = random()
        if inCircle(pointX, pointY):
            piCount += 1

    # This is using a quarter of the unit sphere in a 1x1 box.
    # The formula is pi = (boxLength^2 / radius^2) * (piCount / n), but we
    # are only using the upper quadrant and the unit circle, so we can use
    # 4*piCount/n instead
    # piEstimate = 4*piCount/n
    piEstimate = 4 * piCount / n
    print('Pi is {0:} ({1:.4f}% error)'.format(
        piEstimate, (pi - piEstimate) / pi * 100))


if __name__ == "__main__":
    monteCarlo(100000)
