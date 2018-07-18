#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct point {
    double x, y;
};

int cmp_points(const void *a, const void *b) {
    struct point* pa = (struct point*) a;
    struct point* pb = (struct point*) b;

    if (pa->y > pb->y) {
        return 1;
    } else if (pa->y < pb->y) {
        return -1;
    } else {
        return 0;
    }
}

double ccw(struct point a, struct point b, struct point c) {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

double polar_angle(struct point origin, struct point p) {
    return atan2(p.y - origin.y, p.x - origin.x);
}

void polar_angles_sort(struct point *points, struct point origin, size_t size) {
    if (size < 2) {
        return;
    }

    double pivot_angle = polar_angle(origin, points[size / 2]);

    int i = 0;
    int j = size - 1;
    while (1) {
        while (polar_angle(origin, points[i]) < pivot_angle) {
            i++;
        }
        while (polar_angle(origin, points[j]) > pivot_angle) {
            j--;
        }

        if (i >= j) {
            break;
        }

        struct point tmp = points[i];
        points[i] = points[j];
        points[j] = tmp;

        i++;
        j--;
    }

    polar_angles_sort(points, origin, i);
    polar_angles_sort(points + i, origin, size - i);
}

size_t graham_scan(struct point *points, size_t size) {
    qsort(points, size, sizeof(struct point), cmp_points);
    polar_angles_sort(points, points[0], size);

    struct point tmp_points[size + 1];
    memcpy(tmp_points + 1, points, size * sizeof(struct point));
    tmp_points[0] = tmp_points[size];

    size_t m = 1;
    for (size_t i = 2; i <= size; ++i) {
        while (ccw(tmp_points[m - 1], tmp_points[m], tmp_points[i]) <= 0) {
            if (m > 1) {
                m--;
                continue;
            } else if (i == size) {
                break;
            } else {
                i++;
            }
        }

        m++;
        struct point tmp = tmp_points[i];
        tmp_points[i] = tmp_points[m];
        tmp_points[m] = tmp;
    }

    memcpy(points, tmp_points + 1, size * sizeof(struct point));

    return m;
}

int main() {
    struct point points[] = {{2.0, 1.9}, {1.0, 1.0}, {2.0, 4.0}, {3.0, 1.0},
                                {2.0, 0.0}};

    printf("Points:\n");
    for (size_t i = 0; i < 5; ++i) {
        printf("(%f,%f)\n", points[i].x, points[i].y);
    }

    size_t hull_size = graham_scan(points, 5);

    printf("\nHull:\n");
    for (size_t i = 0; i < hull_size; ++i) {
        printf("(%f,%f)\n", points[i].x, points[i].y);
    }

    return 0;
}
