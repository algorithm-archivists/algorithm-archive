# Author: gammison

# note this example is inplace and destructive
def thomas(a, b, c, d):

    # set the initial elements
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    n = len(d) # number of equations to solve
    for i in range(1, n):
        # scale factor for c and d
        scale = 1 / (b[i] - c[i-1] * a[i])

        c[i] *= scale
        d[i] = (d[i] - a[i] * d[i-1]) * scale


    # do the back substitution
    for i in range(n-2, -1, -1):
        d[i] -= c[i] * d[i+1]

    return d

def main():
    # example for matrix
    # [1  4  0][x]   [7]
    # [2  3  5][y] = [5]
    # [0  3  6][z]   [3]

    #                 [.8666]
    # soln will equal [1.533]
    #                 [-.266]
    # note we index a from 1 and c from 0
    a = [0, 2, 3]
    b = [1, 3, 6]
    c = [4, 5, 0]
    d = [7, 5, 3]

    soln = thomas(a, b, c, d)
    print(soln)

if __name__ == '__main__':
    main()
