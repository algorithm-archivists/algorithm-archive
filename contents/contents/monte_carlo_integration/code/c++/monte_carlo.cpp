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
    static std::uniform_real_distribution<double> dist(0, 1);

    unsigned count = 0;
    for (unsigned i = 0; i < samples; ++i) {
        double x = dist(generator);
        double y = dist(generator);

        if (in_circle(x, y))
            ++count;
    }

    return 4.0 * count / samples;
}

int main() {
    unsigned samples;

    std::cout << "Enter samples to use: ";
    std::cin >> samples;

    double pi_estimate = monte_carlo_pi(samples);
    std::cout << "Pi = " << pi_estimate << '\n';
    std::cout << "Percent error is: " << 100 * std::abs(pi_estimate - PI) / PI << " %\n";
}
