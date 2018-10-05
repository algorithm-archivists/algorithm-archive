#include <math.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

bool in_circle(double x, double y, double radius) {
    return x * x + y * y < radius * radius;
}

void monte_carlo(int samples) {
    double radius = 1.0;
    int count = 0;

    for (int i = 0; i < samples; ++i) {
        double x = (double)rand() * radius / RAND_MAX;
        double y = (double)rand() * radius / RAND_MAX;

        if (in_circle(x, y, radius)) {
            count += 1;
        }
    }

    double estimate = 4.0 * count / (samples * radius * radius);

    printf("The estimate of pi is %f\n", estimate);
    printf("Percentage error: %0.2f%\n", 100 * fabs(M_PI - estimate) / M_PI);
}

int main() {
    srand(time(NULL));

    double estimate = monte_carlo(1000000);
	
    printf("The estimate of pi is %f\n", estimate);
    printf("Percentage error: %0.2f%\n", 100 * fabs(M_PI - estimate) / M_PI);
	
    return 0;
}
