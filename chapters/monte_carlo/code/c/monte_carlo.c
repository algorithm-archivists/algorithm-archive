#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

bool in_circle(double x, double y, double radius) {
    return x * x + y * y < radius * radius;
}

void monte_carlo(int samples, double radius) {
    int cnts = 0.0;

    for (int i = 0; i < samples; ++i) {
        double x = (double)rand() * 2.0 / RAND_MAX - 1.0;
        double y = (double)rand() * 2.0 / RAND_MAX - 1.0;

        if (in_circle(x, y, radius)) {
            cnts += 1;
        }
    }

    printf("the estimate of pi is %f\n",
           4.0 * cnts / (samples * radius * radius));
}

int main() {
    srand(time(NULL));

    monte_carlo(1000000, 1.0);

    return 0;
}
