#include <cmath>
#include <iostream>
#include <numeric>
#include <random>

// Returns a pseudo-random number generator
std::default_random_engine& rng() {
  // Initialize static pseudo-random engine with non-deterministic random seed
  static std::default_random_engine randEngine(std::random_device{}());
  return randEngine;
}

// Returns a random double in [0, 1)
double drand() {
  return std::uniform_real_distribution<double>(0.0, 1.0)(rng());
}

// This function takes
//     - v: value in register
//     - a: a  scaling value for the logarithm based on Morris's paper
// It returns n(v,a), the approximate count
auto n(double v, double a) { return a * (pow((1 + 1 / a), v) - 1); }

// This function takes
//    - v: value in register
//    - a: a scaling value for the logarithm based on Morris's paper
// It returns a new value for v
auto increment(int v, double a) {
  // delta is the probability of incrementing our counter
  const auto delta = 1 / (n(v + 1, a) - n(v, a));
  return (drand() <= delta) ? v + 1 : v;
}

// This simulates counting and takes
//     - n_items: number of items to count and loop over
//     - a: a scaling value for the logarithm based on Morris's paper
// It returns n(v,a), the approximate count
auto approximate_count(int n_items, double a) {
  auto v = 0;
  for (auto i = 0; i < n_items; ++i)
    v = increment(v, a);

  return n(v, a);
}

// This function takes
//     - n_trials: the number of counting trials
//     - n_items: the number of items to count to
//     - a: a scaling value for the logarithm based on Morris's paper
//     - threshold: the maximum percent error allowed
// It returns a "pass" / "fail" test value
auto test_approximate_count(
    int n_trials, int n_items, double a, double threshold) {
  auto sum = 0.0;
  for (auto i = 0; i < n_trials; ++i)
    sum += approximate_count(n_items, a);
  const auto avg = sum / n_trials;
  return std::abs((avg - n_items) / n_items) < threshold ? "passed" : "failed";
}

int main() {
  std::cout << "[#]\nCounting Tests, 100 trials\n";

  std::cout << "[#]\ntesting 1,000, a = 30, 10% error \n"
            << test_approximate_count(100, 1000, 30, 0.1) << "\n";
  std::cout << "[#]\ntesting 12,345, a = 10, 10% error \n"
            << test_approximate_count(100, 12345, 10, 0.1) << "\n";
  // Note : with a lower a, we need more trials, so a higher % error here.
  std::cout << "[#]\ntesting 222,222, a = 0.5, 20% error \n"
            << test_approximate_count(100, 222222, 0.5, 0.2) << "\n";
}
