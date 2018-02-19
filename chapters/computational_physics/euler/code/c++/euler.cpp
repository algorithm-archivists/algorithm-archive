#include <iostream>
#include <cmath>
#include <vector>
#include <cstddef>
#include <iterator>
#include <algorithm>
#include <utility>

using std::begin;
using std::end;

using std::size_t;

std::vector<double> solve_euler(double timestep, size_t size) {
  std::vector<double> result;
  double current = 1.0;
  std::generate_n(
    std::back_inserter(result),
    size,
    [&] { return std::exchange(current, current - 3.0 * current * timestep); });
  return result;
}

/*
  check_result takes an iterator over doubles,
  and returns whether any value is outside the passed threshold.
*/
template <typename Iter>
bool check_result(Iter first, Iter last, double threshold, double timestep) {
  auto it = first;
  for (size_t idx = 0; it != last; ++idx, ++it) {
    double solution = std::exp(-3.0 * idx * timestep);
    if (std::abs(*it - solution) > threshold) {
      std::cout
        << "We found a value outside the threshold; the "
        << idx
        << "-th value was "
        << *it
        << ", but the expected solution was "
        << solution
        << '\n';
      std::cout
        << "(the threshold was "
        << threshold
        << " and the difference was "
        << std::abs(*it - solution)
        << ")\n";
      return true;
    }
  }
  return false;
}

int main(){
  double threshold = 0.01;
  double timestep = 0.01;

  auto result = solve_euler(timestep, 100);
  auto outside_threshold =
    check_result(begin(result), end(result), threshold, timestep);
  auto msg = outside_threshold ? "yes :(" : "no :D";

  std::cout
    << "Were any of the values outside of the threshold ("
    << threshold
    << ")? "
    << msg
    << '\n';
}
