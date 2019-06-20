#include<iostream>
#include<vector>

struct point {
  double x;
  double y;
};

point origin;

void swap(point &p1, point &p2){
  point temp = p1;
  p1 = p2;
  p2 = temp;
}

int distSq(point p1, point p2){
  return (p1.x - p2.x)*(p1.x - p2.x) +(p1.y - p2.y)*(p1.y - p2.y);
}

int ccwCheck(point a, point b, point c){

  int crossProduct = (b.y - a.y) * (c.x - b.x) - (b.x - a.x) * (c.y - b.y);
  if (crossProduct == 0)
  {
    return 0;
  }
  return ( crossProduct > 0)? -1 : 1;
}

int compare(const void *vp1, const void *vp2){

   point *p1 = (point *)vp1;
   point *p2 = (point *)vp2;

   int o = ccwCheck(origin, *p1, *p2);
   if (o == 0)
     return (distSq(origin, *p2) >= distSq(origin, *p1))? -1 : 1;

   return (o == 1)? -1: 1;
}

void print(std::vector<point> points)
{
  std::cout << "the points of hull are as follows:\n";
  for (size_t i = 0; i < points.size(); i++) {
    std::cout << "(" << points[i].x << "," << points[i].y << ")\n";
  }
}

std::vector<point> grahamScan(std::vector<point> points){
  //selecting origin(the element with y minimum)
  double yMin = points[0].y;
  int min = 0;
  for (size_t i = 1; i < points.size(); i++) {
    double y = points[i].y;
    if ((y < yMin) || (yMin == y && points[i].x < points[min].x)){
      yMin = points[i].y;
      min = i;
    }
  }
  swap(points[0], points[min]);
  origin = points[0];

  //sorting by polar angle and removing duplicates
  qsort(&points[1], points.size() - 1, sizeof(point), compare);
  std::vector<point> pointsSorted;
  pointsSorted.push_back(origin);
  for(size_t i = 1; i < points.size(); i++){
    while(i < points.size() - 1 && ccwCheck(origin, points[i],
      points[i+1]) == 0){
      i++;
    }
    pointsSorted.push_back(points[i]);
  }

  //creating the convex hull
  std::vector<point> hull;
  hull.push_back(points[0]);
  hull.push_back(points[1]);
  hull.push_back(points[2]);
  for (size_t i = 3; i <= pointsSorted.size(); i++) {
    while (ccwCheck(hull.at(hull.size()-2), hull.at(hull.size()-1),
    pointsSorted[i]) != 1)
      {
        hull.pop_back();
      }
      hull.push_back(points[i]);
  }
  return hull;
}

int main()
{
  std::vector<point> points = {{-5, 2}, {5, 7}, {-6, -12}, {-14, -14}, {9, 9},
                              {-1, -1}, {-10, 11}, {-6, 15}, {-6, -8}, {15, -9},
                              {7, -7}, {-2, -9}, {6, -5}, {0, 14}, {2, 8}};

  std::vector<point> hull = grahamScan(points);
  print(hull);
  return 0;
}
