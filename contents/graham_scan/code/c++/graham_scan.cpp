#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

struct point {
  double x;
  double y;
};

std::ostream& operator<<(std::ostream& os, const std::vector<point>& points) {
  for (auto p : points) {
    os << "(" << p.x << ", " << p.y << ")\n";
  }
  return os;
}

double ccw(const point& a, const point& b, const point& c) {
  return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

double polar_angle(const point& origin, const point& p) {
  return std::atan2(p.y - origin.y, p.x - origin.x);
}

std::vector<point> graham_scan(std::vector<point>& points) {
  // selecting lowest point as pivot
  size_t low_index = 0;
  for (size_t i = 1; i < points.size(); i++) {
    if (points[i].y < points[low_index].y) {
      low_index = i;
    }
  }
  std::swap(points[0], points[low_index]);
  point pivot = points[0];

  // sorting points by polar angle
  std::sort(
      points.begin() + 1,
      points.end(),
      [&pivot](const point& pa, const point& pb) {
        return polar_angle(pivot, pa) < polar_angle(pivot, pb);
      });

  // creating convex hull
  size_t m = 1;
  for (size_t i = 2; i < points.size(); i++) {
    while (ccw(points[m - 1], points[m], points[i]) <= 0) {
      if (m > 1) {
        m--;
        continue;
      } else if (i == points.size()) {
        break;
      } else {
        i++;
      }
    }
    m++;
    std::swap(points[i], points[m]);
  }
  return std::vector<point>(points.begin(), points.begin() + m + 1);
}

int main() {
  std::vector<point> points = {{-5, 2},
                               {5, 7},
                               {-6, -12},
                               {-14, -14},
                               {9, 9},
                               {-1, -1},
                               {-10, 11},
                               {-6, 15},
                               {-6, -8},
                               {15, -9},
                               {7, -7},
                               {-2, -9},
                               {6, -5},
                               {0, 14},
                               {2, 8}};
  std::cout << "original points are as follows:\n" << points;
  const std::vector<point> hull = graham_scan(points);
  std::cout << "points in hull are as follows:\n" << hull;
  return 0;
}
