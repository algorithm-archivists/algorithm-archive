#include <stdio.h>
#include <string.h>

void thomas(double * const a, double * const b, double * const c,
            double * const x, const size_t size) {

    double y[size];
    memset(y, 0, size * sizeof(double));

    y[0] = c[0] / b[0];
    x[0] = x[0] / b[0];

    for (size_t i = 1; i < size; ++i) {
        double scale = 1.0 / (b[i] - a[i] * y[i - 1]);
        y[i] = c[i] * scale;
        x[i] = (x[i] - a[i] * x[i - 1]) * scale;
    }

    for (int i = size - 2; i >= 0; --i) {
        x[i] -= y[i] * x[i + 1];
    }
}

int main() {
    double a[] = {0.0, 2.0, 3.0};
    double b[] = {1.0, 3.0, 6.0};
    double c[] = {4.0, 5.0, 0.0};
    double x[] = {7.0, 5.0, 3.0};

    printf("The system,\n");
    printf("[1.0  4.0  0.0][x] = [7.0]\n");
    printf("[2.0  3.0  5.0][y] = [5.0]\n");
    printf("[0.0  3.0  6.0][z] = [3.0]\n");
    printf("has the solution:\n");

    thomas(a, b, c, x, 3);

    for (size_t i = 0; i < 3; ++i) {
        printf("[%f]\n", x[i]);
    }

    return 0;
}
