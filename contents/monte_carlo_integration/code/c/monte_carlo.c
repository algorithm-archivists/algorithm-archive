#include <math.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

bool in_circle(double x, double y) {
    return x * x + y * y < 1;
}

double monte_carlo(unsigned int samples) {
    unsigned int count = 0;

    for (unsigned int i = 0; i < samples; ++i) {
        double x = (double)rand() / RAND_MAX;
        double y = (double)rand() / RAND_MAX;

        if (in_circle(x, y)) {
            count += 1;
        }
    }

    return 4.0 * count / samples;
}

int main() {
    srand(time(NULL));

    double estimate = monte_carlo(1000000);

    printf("The estimate of pi is %g\n", estimate);
    printf("Percentage error: %0.2f%%\n", 100 * fabs(M_PI - estimate) / M_PI);

    return 0;
}
