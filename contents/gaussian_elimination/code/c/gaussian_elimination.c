#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void swap_rows(double * const a, size_t i, size_t pivot, size_t cols) {
    for (size_t j = 0; j < cols; ++j) {
        double tmp = a[i * cols + j];
        a[i * cols + j] = a[pivot * cols + j];
        a[pivot * cols + j] = tmp;
    }
}

void gaussian_elimination(double *a, const size_t rows, const size_t cols) {
    size_t min_dim = (rows < cols)? rows: cols;

    for (size_t k = 0; k < min_dim; ++k) {
        size_t pivot = k;

        for (size_t i = k + 1; i < rows; ++i) {
            if (fabs(a[i * cols + k]) > fabs(a[pivot * cols + k])) {
                pivot = i;
            }
        }

        if (a[pivot * cols + k] == 0) {
            printf("The matrix is singular.\n");
            exit(0);
        }

        if (k != pivot) {
            swap_rows(a, k, pivot, cols);
        }

        for (size_t i = k + 1; i < rows; ++i) {
            double scale = a[i * cols + k] / a[k * cols + k];

            for (size_t j = k + 1; j < cols; ++j) {
                a[i * cols + j] -= a[k * cols + j] * scale;
            }

            a[i * cols + k] = 0;
        }
    }
}

void back_substitution(const double * const a, double * const x,
                       const size_t rows, const size_t cols) {

    for (int i = rows - 1; i >= 0; --i) {
        double sum = 0.0;

        for (size_t j = cols - 2; j > i; --j) {
            sum += x[j] * a[i * cols + j];
        }

        x[i] = (a[i * cols + cols - 1] - sum) / a[i * cols + i];
    }
}

int main() {
    double a[3][4] = {{3.0, 2.0, -4.0, 3.0},
                      {2.0, 3.0, 3.0, 15.0},
                      {5.0, -3.0, 1.0, 14.0}};

    gaussian_elimination((double *)a, 3, 4);

    for (size_t i = 0; i < 3; ++i) {
        printf("[");
        for (size_t j = 0; j < 4; ++j) {
            printf("%f ", a[i][j]);
        }
        printf("]\n");
    }

    printf("\n");

    double x[3] = {0, 0, 0};
    back_substitution((double *)a, x, 3, 4);

    printf("(%f,%f,%f)\n", x[0], x[1], x[2]);

    return 0;
}
