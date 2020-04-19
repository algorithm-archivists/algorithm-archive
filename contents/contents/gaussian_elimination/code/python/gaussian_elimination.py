import numpy as np

def gaussian_elimination(A):

    pivot_row = 0
    
    # Go by column
    for pivot_col in range(min(A.shape[0], A.shape[1])):

        # Swap row with highest element in col
        max_i = np.argmax(abs(A[pivot_row:, pivot_col])) + pivot_row
        
        temp = A[pivot_row, :].copy()
        A[pivot_row, :] = A[max_i, :]
        A[max_i, :] = temp

        # Skip on singular matrix,  not actually a pivot
        if A[pivot_row, pivot_col] == 0:
            continue

        # Zero out elements below pivot
        for r in range(pivot_row + 1,  A.shape[0]):
            # Get fraction
            frac = -A[r, pivot_col] / A[pivot_row, pivot_col]
            # Add rows
            A[r, :] += frac * A[pivot_row, :]

        pivot_row += 1


# Assumes A is already row echelon form
def gauss_jordan_elimination(A):
    
    col = 0

    # Scan for pivots
    for row in range(A.shape[0]):
        while col < A.shape[1] and A[row, col] == 0:
            col += 1
            
        if col >= A.shape[1]:
            continue

        # Set each pivot to one via row scaling
        A[row, :] /= A[row, col]

        # Zero out elements above pivot
        for r in range(row):
            A[r, :] -= A[r, col] * A[row, :]


# Assumes A has a unique solution and A in row echelon form
def back_substitution(A):
    
    sol = np.zeros(A.shape[0]).T

    # Go by pivots along diagonal
    for pivot_i in range(A.shape[0] - 1,  -1,  -1):
        s = 0
        for col in range(pivot_i + 1,  A.shape[1] - 1):
            s += A[pivot_i, col] * sol[col]
        sol[pivot_i] = (A[pivot_i, A.shape[1] - 1] - s) / A[pivot_i, pivot_i]
        
    return sol


def main():
    A = np.array([[2, 3, 4, 6],
                  [1, 2, 3, 4,],
                  [3, -4, 0, 10]], dtype=float)

    print("Original")
    print(A, "\n")

    gaussian_elimination(A)
    print("Gaussian elimination")
    print(A, "\n")

    print("Back subsitution")
    print(back_substitution(A), "\n")

    gauss_jordan_elimination(A)
    print("Gauss-Jordan")
    print(A, "\n")


if __name__ == "__main__":
    main()

