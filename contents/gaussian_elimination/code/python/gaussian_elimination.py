import numpy as np

def gaussian_elimination(A):
    # Step 1: Go by number of pivots with end being the min number of rows or cols
    for pivot_i in range(min(A.shape[0], A.shape[1])):

        # Step 2: Swap row with highest element in col
        max_i = np.argmax(abs(A[pivot_i:,pivot_i])) + pivot_i
        
        temp = A[pivot_i,:].copy()
        A[pivot_i,:] = A[max_i,:]
        A[max_i,:] = temp

        if A[pivot_i, pivot_i] == 0:
            # Skip eval on singular matrix
            continue

        for row in range(pivot_i+1, A.shape[0]):
            # Step 3: Get fraction
            frac = -A[row,pivot_i] / A[pivot_i,pivot_i]
            # Step 4: Add rows
            A[row,:] = A[row,:] + frac*A[pivot_i,:]

"""
Assumes A is already ref
"""
def gauss_jordon_elimination(A):
    # Find and set each pivot to one via row scaling
    col = 0
    for row in range(A.shape[0]):
        while A[row,col] == 0:
            col += 1
            if col >= A.shape[1]:
                break
        if col >= A.shape[1]:
            continue
        A[row,:] = A[row,:] / A[row,col]

        # kill rows above
        for r in range(row):
            A[r,:] = A[r,:] - A[r,col] * A[row,:]


"""
Assumes A has a unique solution and A in ref
"""
def back_sub(A):
    sol = np.zeros(A.shape[0]).T
    for pivot_i in range(A.shape[0]-1, -1, -1):
        s = 0
        for col in range(pivot_i+1, A.shape[1]-1):
            s += A[pivot_i,col] * sol[col]
        sol[pivot_i] = (A[pivot_i,A.shape[1]-1] - s) / A[pivot_i,pivot_i]
    return sol


# MAIN
A = np.matrix('0. 2 1 -8; 1 -2 -3 0; -1 1 2 3')
#A = np.matrix('1. -2 -6 12; 2 4 12 -17; 1 -4 -12 22')
print(A)
print()

gaussian_elimination(A)
print("ref")
print(A)
print()

print("back sub")
print(back_sub(A))
print()

gauss_jordon_elimination(A)
print("rref")
print(A)
print()
