// The code bellow uses C++-17 features, compile it with C++-17 flags, e.g.:
// clang++ -Wall -Wextra -Wshadow -Wnon-virtual-dtor -Wold-style-cast -Wcast-align -Wunused -Woverloaded-virtual -Wpedantic -Wconversion -Wsign-conversion -Wnull-dereference -Wdouble-promotion -Wformat=2 -gdwarf-3 -D_GLIBCXX_DEBUG -std=c++17 -O3 -c ./barnsley.cpp barnsley

#include <array>
#include <cassert>
#include <fstream>
#include <random>

using Vec2 = std::array<double, 2>;
using Vec3 = std::array<double, 3>;
using Row = std::array<double, 3>;
using Op = std::array<Row, 3>;

constexpr auto OpN = 4U;

template <size_t N>
auto operator+(std::array<double, N> x, std::array<double, N> y) {
  for (auto i = 0U; i < N; ++i)
    x[i] += y[i];
  return x;
}

template <size_t N>
auto operator*(double k, std::array<double, N> v) {
  for (auto i = 0U; i < N; ++i)
    v[i] *= k;
  return v;
}

template <size_t N>
auto operator*(std::array<double, N> v, double k) {
  return k * v;
}

auto operator*(const Op& x, const Vec3& y) {
  auto ret = Vec3{};
  for (auto i = 0U; i < 3U; ++i) {
    ret[i] = 0;
    for (auto j = 0U; j < 3U; ++j)
      ret[i] += y[j] * x[i][j];
  }
  return ret;
}

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

// This is a function that reads in the Hutchinson operator and
// corresponding
//     probabilities and outputs a randomly selected transform
// This works by choosing a random number and then iterating through all
//     probabilities until it finds an appropriate bin
auto select_array(
    const std::array<Op, OpN>& hutchinson_op,
    const std::array<double, OpN>& probabilities) {

  // random number to be binned
  auto rnd = drand();

  // This checks to see if a random number is in a bin, if not, that
  //     probability is subtracted from the random number and we check the
  //     next bin in the list
  for (auto i = 0U; i < probabilities.size(); ++i) {
    if (rnd < probabilities[i])
      return hutchinson_op[i];
    rnd -= probabilities[i];
  }
  assert(!static_cast<bool>("check if probabilities adding up to 1"));
  return hutchinson_op[0];
}

// This is a general function to simulate a chaos game
// n is the number of iterations
// initial_location is the the starting point of the chaos game
// hutchinson_op is the set of functions to iterate through
// probabilities is the set of probabilities corresponding to the likelihood
//     of choosing their corresponding function in hutchinson_op
auto chaos_game(
    size_t n,
    Vec2 initial_location,
    const std::array<Op, OpN>& hutchinson_op,
    const std::array<double, OpN>& probabilities) {

  // Initializing the output array and the initial point
  auto output_points = std::vector<Vec2>{};

  // extending point to 3D for affine transform
  auto point = Vec3{initial_location[0], initial_location[1], 1};

  for (auto i = 0U; i < n; ++i) {
    output_points.push_back(Vec2{point[0], point[1]});
    point = select_array(hutchinson_op, probabilities) * point;
  }

  return output_points;
}

int main() {

  const std::array barnsley_hutchinson = {
      Op{Row{0.0, 0.0, 0.0}, Row{0.0, 0.16, 0.0}, Row{0.0, 0.0, 1.0}},
      Op{Row{0.85, 0.04, 0.0}, Row{-0.04, 0.85, 1.60}, Row{0.0, 0.0, 1.0}},
      Op{Row{0.20, -0.26, 0.0}, Row{0.23, 0.22, 1.60}, Row{0.0, 0.0, 1.0}},
      Op{Row{-0.15, 0.28, 0.0}, Row{0.26, 0.24, 0.44}, Row{0.0, 0.0, 1.0}}};

  const std::array barnsley_probabilities = {0.01, 0.85, 0.07, 0.07};
  auto output_points = chaos_game(
      10'000, Vec2{0, 0}, barnsley_hutchinson, barnsley_probabilities);

  std::ofstream ofs("out.dat");
  for (auto pt : output_points)
    ofs << pt[0] << '\t' << pt[1] << '\n';
}
