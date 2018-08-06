#include <iostream>
#include <cstdlib>
#include <random>

constexpr double PI = 3.14159265358979323846264338;

/**
 * Check if the point (x, y) is within a circle of a given radius.
 * @param x coordinate one
 * @param y coordinate two
 * @param r radius of the circle (optional)
 * @return true if (x, y) is within the circle.
 */
inline bool in_circle(double x, double y, double r = 1) {
    return x * x + y * y < r * r;
}

/**
 * Return an estimate of PI using Monte Carlo integration.
 * @param samples number of iterations to use
 * @return estimate of pi
 */
double monte_carlo_pi(unsigned samples) {
    static std::default_random_engine generator;
    static std::uniform_real_distribution<double> dist(-1, 1);

    unsigned count = 0;
    for (unsigned i = 0; i < samples; ++i) {
        double x = dist(generator);
        double y = dist(generator);

        if (x*x + y*y < 1)
            ++count;
    }

    return 4.0 * count / (double) samples;
}

int main() {
    unsigned samples;

    std::cout << "Enter samples to use: ";
    std::cin >> samples;

    double pi = monte_carlo_pi(samples);
    printf("Pi = %f\n", pi);
    printf("Percent error is: %g %%\n", 100 * std::abs(pi - PI) / PI);
}
