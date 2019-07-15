#include <iostream>
#include <vector>
#include <algorithm>

struct point{
  double x;
  double y;
};

bool ccw(point a, point b, point c){
  return ((b.x - a.x)*(c.y - a.y) > (b.y - a.y)*(c.x - a.x));
}

std::vector<point> grahamScan(std::vector<point> points){
  //selecting lowest point as pivot
  int lowIndex = 0;
  for(size_t i = 1; i < points.size(); i++) {
    if(points[i].y < points[lowIndex].y){
      lowIndex = i;
    }
  }
  std::swap(points[0], points[lowIndex]);
  point pivot = points[0];

  //sorting points by polar angle
  std::sort(points.begin()+1, points.end(), [pivot](point a, point b){
    return ccw(pivot, a, b);
  });

  //creating convex hull
  size_t m = 1;
  for(size_t i = 2; i < points.size(); i++) {
    while(ccw(points[m - 1], points[m], points[i]) <= 0){
      if(m > 1) {
                m--;
                continue;
            } else if(i == points.size()) {
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

void print(std::vector<point> points){
  for ( auto p : points){
    std::cout <<"(" << p.x << ", " << p.y <<")\n";
  }
}

int main(){
  std::vector<point> points = {{-5, 2}, {5, 7}, {-6, -12}, {-14, -14}, {9, 9},
                              {-1, -1}, {-10, 11}, {-6, 15}, {-6, -8}, {15, -9},
                              {7, -7}, {-2, -9}, {6, -5}, {0, 14}, {2, 8}};
  std::cout << "original points are as follows:\n";
  print(points);
  std::vector<point> hull = grahamScan(points);
  std::cout << "points in hull are as follows:\n";
  print(hull);
  return 0;
}
