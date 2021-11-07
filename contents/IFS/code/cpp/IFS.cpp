#include <cmath>
#include <fstream>
#include <random>
#include <vector>

// Simple X-Y point structure, along with some operators
struct Point {
  double x, y;
};

Point operator+(Point lhs, Point rhs) { return {lhs.x + rhs.x, lhs.y + rhs.y}; }
Point operator*(double k, Point pt) { return {k * pt.x, k * pt.y}; }
Point operator*(Point pt, double k) { return k * pt; }

using PointVector = std::vector<Point>;

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

// Returns a random integer in [0, numElems-1]
std::size_t randrange(std::size_t numElems) {
  return std::uniform_int_distribution<std::size_t>(0, numElems - 1)(rng());
}

// Return a random point from the non-empty PointVector
Point choose(const PointVector& points) {
  return points[randrange(points.size())];
}

// This is a function to simulate a "chaos game"
PointVector chaosGame(int numOutputPoints, const PointVector& inputPoints) {
  // Choose first point randomly
  Point curPoint = {drand(), drand()};

  // For each output point, compute midpoint to random input point
  PointVector outputPoints(numOutputPoints);
  for (auto& outPoint : outputPoints) {
    outPoint = curPoint;
    curPoint = 0.5 * (curPoint + choose(inputPoints));
  }

  return outputPoints;
}

int main() {
  // This will generate a Sierpinski triangle with a chaos game of n points for
  // an initial triangle with three points on the vertices of an equilateral
  // triangle.
  PointVector inputPoints = {{0.0, 0.0}, {0.5, std::sqrt(0.75)}, {1.0, 0.0}};
  auto outputPoints = chaosGame(10000, inputPoints);

  // It will output the file sierpinski.dat, which can be plotted after
  std::ofstream ofs("sierpinski.dat");
  for (auto pt : outputPoints)
    ofs << pt.x << '\t' << pt.y << '\n';
}
