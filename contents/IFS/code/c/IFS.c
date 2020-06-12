#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct point {
    double x, y;
};

double drand() {
    return ((double) rand() / (RAND_MAX));
}

struct point random_element(struct point *array, size_t n) {
    return array[rand() % (int)n];
}

void chaos_game(struct point *in, size_t in_n, struct point *out,
                size_t out_n) {

    struct point cur_point = {drand(), drand()};

    for (int i = 0; i < out_n; ++i) {
        out[i] = cur_point;
        struct point tmp = random_element(in, in_n);
        cur_point.x = 0.5 * (cur_point.x + tmp.x);
        cur_point.y = 0.5 * (cur_point.y + tmp.y);
    }
}

int main() {
    struct point shape_points [3] = {{0.0,0.0}, {0.5,sqrt(0.75)}, {1.0,0.0}};
    struct point out_points[1000];

    srand(time(NULL));

    chaos_game(shape_points, 3, out_points, 1000);

    FILE *fp = fopen("out.dat", "w+");

    for (int i = 0; i < 1000; ++i) {
        fprintf(fp, "%f\t%f\n", out_points[i].x, out_points[i].y);
    }

    fclose(fp);

    return 0;
}

