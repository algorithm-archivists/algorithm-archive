from random import random, choice
from math import sqrt

# This is a function to simulate a "chaos game"
def chaos_game(n, shape_points, output_file = "out.dat"):
    # Initialize the starting point
    point = [random(), random()]

    with open(output_file, "w") as f:
        for _ in range(n):
            # Update the point position and write it to the file
            point = [_p + _s for _p, _s in zip(point, choice(shape_points))]
            f.write(",".join([str(_p) for _p in p]) + '\n')

# This will generate a Sierpinski triangle with a chaos game of n points for an
# initial triangle with three points on the vertices of an equilateral triangle:
#     A = (0.0, 0.0)
#     B = (0.5, sqrt(0.75))
#     C = (1.0, 0.0)
# It will output the file sierpinski.dat, which can be plotted after
shape_points = [[0.0, 0.0],
                [0.5, sqrt(0.75)],
                [1.0, 0.0]]
chaos_game(10000, shape_points, 'sierpinski.dat')
