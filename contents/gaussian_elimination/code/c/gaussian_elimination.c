#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void swap_rows(double *a, const size_t i, const size_t pivot,
               const size_t cols) {

    for (size_t j = 0; j < cols; ++j) {
        double tmp = a[i * cols + j];
        a[i * cols + j] = a[pivot * cols + j];
        a[pivot * cols + j] = tmp;
    }
}

void gaussian_elimination(double *a, const size_t rows, const size_t cols) {
    size_t row = 0;

    for (size_t col = 0; col < cols - 1; ++col) {
        size_t pivot = row;

        for (size_t i = row + 1; i < rows; ++i) {
            if (fabs(a[i * cols + col]) > fabs(a[pivot * cols + col])) {
                pivot = i;
            }
        }

        if (a[pivot * cols + col] == 0) {
            printf("The matrix is singular.\n");
            continue;
        }

        if (col != pivot) {
            swap_rows(a, col, pivot, cols);
        }

        for (size_t i = row + 1; i < rows; ++i) {
            double scale = a[i * cols + col] / a[row * cols + col];

            for (size_t j = col + 1; j < cols; ++j) {
                a[i * cols + j] -= a[row * cols + j] * scale;
            }

            a[i * cols + col] = 0;
        }

        row++;
    }
}

void back_substitution(const double *a, double *x, const size_t rows,
                       const size_t cols) {

    for (int i = rows - 1; i >= 0; --i) {
        double sum = 0.0;

        for (size_t j = cols - 2; j > i; --j) {
            sum += x[j] * a[i * cols + j];
        }

        x[i] = (a[i * cols + cols - 1] - sum) / a[i * cols + i];
    }
}

void gauss_jordan(double *a, const size_t rows, const size_t cols) {
    int row = 0;

    for (int col = 0; col < cols - 1; ++col) {
        if (a[row * cols + col] != 0) {
            for (int i = cols - 1; i > col - 1; --i) {
                a[row * cols + i] /= a[row * cols + col];
            }

            for (int i = 0; i < row; ++i) {
                for (int j = cols - 1; j > col - 1; --j) {
                    a[i * cols + j] -= a[i * cols + col] * a[row * cols + j];
                }
            }

            row++;
        }
    }
}

int main() {
    double a[3][4] = {{3.0, 2.0, -4.0, 3.0},
                      {2.0, 3.0, 3.0, 15.0},
                      {5.0, -3.0, 1.0, 14.0}};

    gaussian_elimination((double *)a, 3, 4);

    printf("Gaussian elimination:\n");
    for (size_t i = 0; i < 3; ++i) {
        printf("[");
        for (size_t j = 0; j < 4; ++j) {
            printf("%f ", a[i][j]);
        }
        printf("]\n");
    }

    printf("\nGauss-Jordan:\n");

    gauss_jordan((double *)a, 3, 4);

    for (size_t i = 0; i < 3; ++i) {
        printf("[");
        for (size_t j = 0; j < 4; ++j) {
            printf("%f ", a[i][j]);
        }
        printf("]\n");
    }

    printf("\nSolutions are:\n");

    double x[3] = {0, 0, 0};
    back_substitution((double *)a, x, 3, 4);

    printf("(%f,%f,%f)\n", x[0], x[1], x[2]);

    return 0;
}
