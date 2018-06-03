#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

struct point {
    double x,y;
};

struct point left_most_point(struct point *points, size_t num_points) {
    struct point ret = points[0];

    for (size_t i = 0; i < num_points; ++i) {
        if (points[i].x < ret.x) {
            ret = points[i];
        } else if(points[i].x == ret.x) {
            if (points[i].y < ret.y) {
                ret = points[i];
            }
        }
    }

    return ret;
}

bool equal(struct point a, struct point b) {
    return a.x == b.x && a.y == b.y;
}

double winding(struct point p, struct point q, struct point r) {
    return (q.x - p.x)*(r.y - p.y) - (q.y - p.y)*(r.x - p.x);
}

size_t jarvis_march(struct point *points, struct point *hull_points,
                    size_t num_points) {
    struct point hull_point = left_most_point(points, num_points);
    struct point end_point;

    size_t i = 0;
    do {
        hull_points[i] = hull_point;
        end_point = points[0];

        for (size_t j = 1; j < num_points; ++j) {
            if (equal(end_point, hull_point) ||
                    winding(hull_points[i], end_point, points[j]) > 0.0) {
                end_point = points[j];
            }
        }

        i++;
        hull_point = end_point;
    } while (!equal(end_point, hull_points[0]));

    return i;
}

int main() {
    struct point points[] = {{0.0, 0.0}, {-1.0, -1.0}, {1.0, 1.0}, {0.0, 1.0},
                                {0.0, -1.0}, {2.0, 2.0}};
    struct point hull_points[6];

    size_t num_hull_points = jarvis_march(points, hull_points, 6);

    printf("The Hull points are:\n");
    for (size_t i = 0; i < num_hull_points; ++i) {
        printf("x=%f y=%f\n", hull_points[i].x, hull_points[i].y);
    }

    return 0;
}
