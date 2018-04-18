#include <stdio.h>
#include <math.h>

void solve_euler(double timestep, double *result, size_t n) {
    if (n != 0) {
        result[0] = 1;
        for (size_t i = 1; i < n; ++i) {
            result[i] = result[i-1] - 3.0 * result[i-1] * timestep;
        }
    }
}

int check_result(double *result, size_t n, double threshold, double timestep) {
    int is_approx = 1;
    for (size_t i = 0; i < n; ++i) {
        double solution = exp(-3.0 * i * timestep);
        if (fabs(result[i] - solution) > threshold) {
            printf("%f    %f\n", result[i], solution);
            is_approx = 0;
        }
    }

    return is_approx;
}

int main() {
    double result[100];
    double threshold = 0.01;
    double timestep = 0.01;

    solve_euler(timestep, result, 100);
    printf("%d\n", check_result(result, 100, threshold, timestep));

    return 0;
}
