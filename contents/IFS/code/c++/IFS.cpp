#include <cmath>
#include <fstream>
#include <random>
#include <vector>

struct Point {
  double x, y;
};

Point operator+(Point lhs, Point rhs) { return {lhs.x + rhs.x, lhs.y + rhs.y}; }
Point operator*(double k, Point pt) { return {k * pt.x, k * pt.y}; }
Point operator*(Point pt, double k) { return k * pt; }

template <typename RandEngine>
std::vector<Point> chaosGame(
    int numOutputPoints,
    const std::vector<Point>& inputPoints,
    RandEngine& engine) {

  // Choose first point randomly
  std::uniform_real_distribution<double> doubleDist(0.0, 1.0);
  Point curPoint = {doubleDist(engine), doubleDist(engine)};

  // Prepare local function to choose randomly from input points
  std::uniform_int_distribution<std::size_t> intDist(0, inputPoints.size() - 1);
  auto randInputPoint = [&] { return inputPoints[intDist(engine)]; };

  // For each output point, compute midpoint to random input point
  std::vector<Point> outputPoints(numOutputPoints);
  for (auto& outPoint : outputPoints) {
    outPoint = curPoint;
    curPoint = 0.5 * (curPoint + randInputPoint());
  }

  return outputPoints;
}

int main() {
  // Initialize pseudo-random engine with non-deterministic random seed
  std::default_random_engine randEngine(std::random_device{}());

  // This will generate a Sierpinski triangle with a chaos game of n points for
  // an initial triangle with three points on the vertices of an equilateral
  // triangle.
  std::vector<Point> inputPoints = {
      {0.0, 0.0}, {0.5, std::sqrt(0.75)}, {1.0, 0.0}};
  auto outputPoints = chaosGame(10000, inputPoints, randEngine);

  // It will output the file sierpinski.dat, which can be plotted after
  std::ofstream ofs("sierpinski.dat");
  for (auto pt : outputPoints)
    ofs << pt.x << '\t' << pt.y << '\n';
}
