from random import random, choice
from math import sqrt

# This generator simulates a "chaos game"
def chaos_game(n, shape_points):
    # Initialize the starting point
    point = [random(), random()]

    for _ in range(n):
        # Update the point position and yield the result
        point = [(p + s) / 2 for p, s in zip(point, choice(shape_points))]
        yield point

# This will generate a Sierpinski triangle with a chaos game of n points for an
# initial triangle with three points on the vertices of an equilateral triangle:
#     A = (0.0, 0.0)
#     B = (0.5, sqrt(0.75))
#     C = (1.0, 0.0)
# It will output the file sierpinski.dat, which can be plotted after
shape_points = [[0.0, 0.0],
                [0.5, sqrt(0.75)],
                [1.0, 0.0]]
with open("sierpinski.dat", "w") as f:
    for point in chaos_game(10000, shape_points):
        f.write("{0}\t{1}\n".format(*point))
