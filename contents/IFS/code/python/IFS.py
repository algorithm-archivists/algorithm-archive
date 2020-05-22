from random import random, choice
from math import sqrt, sin, cos, pi

# This generator simulates a "chaos game"
def chaos_game(n, shape_points):
    # Initialize the starting point
    point = [random(), random()]

    for _ in range(n):
        # Update the point position and yield the result
        point = [(_p + _s) / 2 for _p, _s in zip(point, choice(shape_points))]
        yield point

# This function generates the shape points for an N-gon inscribed inside a
#  unit circle, with one of the edges oriented along the bottom
def ngon_shape_points(N):
    # Exterior N-gon angle
    phi = 2 * pi / N

    # Offset to put an edge at the bottom
    delta = (pi + phi) / 2

    return [[cos(- delta - m * phi), sin(- delta - m * phi)] for m in range(N)]

# This will generate a Sierpinski triangle with a chaos game of n points for an
# initial equilateral triangle inscribed on a unit circle. The shape points are:
#     A = (-sqrt(3)/2, -0.5)
#     B = (       0.0,  1.0)
#     C = ( sqrt(3)/2, -0.5)
# It will output the file sierpinski.dat, which can be plotted after
shape_points = ngon_shape_points(3)
with open("sierpinski.dat", "w") as f:
    for point in chaos_game(10000, shape_points):
        f.write("{0},{1}\n".format(*point))
