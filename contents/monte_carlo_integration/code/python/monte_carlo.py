import math
import random


def is_inside(point, radius):
    x, y = point
    return x ** 2 + y ** 2 < radius ** 2


def monte_carlo(samples):
    radius = 1
    count = 0
    for i in range(samples):
        x = random.random()
        y = random.random()

        if is_inside((x, y), radius):
            count += 1
    estimate = 4.0 * count / samples
    print("The estimate of PI is {}".format(estimate))
    print("Which has an error of {}%".format(100. * abs(math.pi - estimate) / math.pi))


if __name__ == '__main__':
    monte_carlo(10000000)
