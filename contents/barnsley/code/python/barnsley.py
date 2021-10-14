import numpy as np
from random import random as rand

# This is a function that reads in the Hutchinson operator and corresponding
#     probabilities and outputs a randomly selected transform
# This works by choosing a random number and then iterating through all
#     probabilities until it finds an appropriate bin
def select_array(hutchinson_op, probabilities):
    # random number to be binned
    rnd = rand()

    # This checks to see if a random number is in a bin, if not, that
    #     probability is subtracted from the random number and we check the
    #     next bin in the list
    for i in range(len(probabilities)):
        if (rnd < probabilities[i]):
            return hutchinson_op[i]
        rnd -= probabilities[i]

# This is a general function to simulate a chaos game
# n is the number of iterations
# initial_location is the starting point of the chaos game
# hutchinson_op is the set of functions to iterate through
# probabilities is the set of probabilities corresponding to the likelihood
#     of choosing their corresponding function in hutchinson_op
def chaos_game(n, initial_location, hutchinson_op, probabilities):

    # Initializing the output array and the initial point
    output_points = np.zeros((n,2))

    # extending point to 3D for affine transform
    point = np.array([initial_location[0], initial_location[1], 1])

    for i in range(n):
        output_points[i] = point[0:2]
        point = select_array(hutchinson_op, probabilities)@point

    return output_points

barnsley_hutchinson = np.array([[[0.0, 0.0, 0.0],
                                 [0.0, 0.16, 0.0],
                                 [0.0, 0.0, 1.0]],
                                [[0.85, 0.04, 0.0],
                                 [-0.04, 0.85, 1.60],
                                 [0.0, 0.0, 1.0]],
                                [[0.20, -0.26, 0.0],
                                 [0.23, 0.22, 1.60],
                                 [0.0, 0.0, 1.0]],
                                [[-0.15, 0.28, 0.0],
                                 [0.26, 0.24, 0.44],
                                 [0.0, 0.0, 1.0]]])

barnsley_probabilities = np.array([0.01, 0.85, 0.07, 0.07])
output_points = chaos_game(10000, [0.0, 0.0], barnsley_hutchinson, barnsley_probabilities)
np.savetxt("out.dat", output_points)
