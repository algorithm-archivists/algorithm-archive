#include <vector>
#include <iostream>
#include <algorithm>

struct Point
{
    double x, y;

    bool operator==(const Point& b) const
    {
        return x == b.x && y == b.y;
    }

    bool operator!=(const Point& b) const
    {
        return !(*this == b);
    }
};

std::vector<Point> jarvis_march(const std::vector<Point>& points)
{
    std::vector<Point> hull_points;

    if(points.empty())
        return hull_points;

    // Left most point
    auto first_point_it = std::min_element(points.begin(), points.end(), [](const Point& a, const Point& b){ return a.x < b.x; });

    auto next_point_it = first_point_it;
    do
    {
        hull_points.push_back(*next_point_it);

        const Point& p1 = hull_points.back();

        // Largest internal angle
        next_point_it = std::max_element(
            points.begin(),
            points.end(),
            [p1](const Point& p2, const Point& p3){
                return (p1 == p2) || (p2.x - p1.x) * (p3.y - p1.y) > (p3.x - p1.x) * (p2.y - p1.y);
            }
        );
    }
    while(*next_point_it != *first_point_it);

    return hull_points;
}

int main() {
    std::vector<Point> points = {
        { 1.0, 3.0 },
        { 1.0, 3.0 },
        { 2.0, 4.0 },
        { 4.0, 0.0 },
        { 1.0, 0.0 },
        { 0.0, 2.0 },
        { 2.0, 2.0 },
        { 3.0, 4.0 },
        { 3.0, 1.0 },
    };

    auto hull_points = jarvis_march(points);

    std::cout << "Hull points are:" << std::endl;

    for(const Point& point : hull_points) {
        std::cout << '(' << point.x << ", " << point.y << ')' << std::endl;
    }
}

