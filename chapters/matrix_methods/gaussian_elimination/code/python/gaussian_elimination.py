import numpy as np

def gaussian_elimination(A):

    rows = len(A)
    cols = len(A[0])
    n    = min(rows, cols)

    # Main loop going through all columns
    for k in range(n):

        # Step 1: finding the maximum element for each column
        max_index = np.argmax(np.abs([A[i][k] for i in range(k,n)])) + k

        # Check to make sure matrix is good!
        if (A[max_index][k] == 0):
            print("matrix is singular! End!")
            return

        # Step 2: swap row with highest value for that column to the top
        A[max_index], A[k] = A[k], A[max_index]

        # Loop for all remaining rows
        for i in range(k+1,rows):

            # Step 3: finding fraction
            fraction = A[i][k] / A[k][k]

            # loop through all columns for that row
            for j in range(k+1,cols):

                 # Step 4: re-evaluate each element
                 A[i][j] -= A[k][j]*fraction

            # Step 5: Set lower elements to 0
            A[i][k] = 0


def back_substitution(A):

    rows = len(A)
    cols = len(A[0])

    # Creating the solution Vector
    soln = [0]*rows

    # initialize the final element
    soln[-1] = A[-1][-1] / A[-1][-2]

    for i  in range(rows - 2,-1,-1):
        sum = 0
        for j in range(rows-1,-1,-1):
            sum += soln[j]*A[i][j]
        soln[i] = (A[i][-1] - sum) / A[i][i]

    return soln

def main():
    A = [[ 2,  3, 4,  6],
         [ 1,  2, 3,  4],
         [ 3, -4, 0, 10]]

    gaussian_elimination(A)
    print(A)
    soln = back_substitution(A)

    for element in soln:
        print(element)

if __name__ == '__main__':
    main()
