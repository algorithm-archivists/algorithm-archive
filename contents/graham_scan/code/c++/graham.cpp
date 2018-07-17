#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

struct Point {
  double x;
  double y;
};

double ccw(const Point& a, const Point& b, const Point& c) {
  return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

std::vector<Point> graham_scan(const std::vector<Point>& input) {
  auto points = input;
  auto const N = points.size();

  // Place the lowest point at the start of the array
  std::iter_swap(
      std::begin(points),
      std::min_element(
          std::begin(points),
          std::end(points),
          [](const Point& a, const Point& b) { return a.y < b.y; }));

  // Sort all other points according to angle with that point
  std::sort(
      std::begin(points) + 1,
      std::end(points),
      [&](const Point& a, const Point& b) {
        return atan2(a.y - points[0].y, a.x - points[0].x) <
               atan2(b.y - points[0].y, b.x - points[0].x);
      });

  // M will be the point on the hull
  auto M = 2u;
  for (auto i = 0u; i < N; ++i) {
    while (ccw(points[M - 2], points[M - 1], points[i]) <= 0) {
      if (M > 2) {
        M -= 1;
        continue;
      }
      // All points are collinear
      if (i == N - 1)
        break;
      ++i;
    }

    // ccw point found, updating hull and swapping points
    M += 1;
    std::swap(points[i], points[M - 1]);
  }

  points.resize(M);
  return points;
}

int main() {
  // This hull is just a simple test so we know what the output should be
  std::vector<Point> points = {{2, 1.9}, {1, 1}, {2, 4}, {3, 1}, {2, 0}};
  auto hull = graham_scan(points);
  for (auto&& p : hull)
    std::cout << "(" << p.x << ", " << p.y << ") ";
  std::cout << std::endl;
}
