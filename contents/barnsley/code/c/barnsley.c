#include <stdio.h>
#include <stdlib.h>

struct matrix {
    double xx, xy, xz,
           yx, yy, yz,
           zx, zy, zz;
};

struct point2d {
    double x, y;
};

struct point3d {
    double x, y, z;
};

struct point3d matmul(struct matrix mat, struct point3d point)
{
    struct point3d out = {
        mat.xx * point.x + mat.xy * point.y + mat.xz * point.z,
        mat.yx * point.x + mat.yy * point.y + mat.yz * point.z,
        mat.zx * point.x + mat.zy * point.y + mat.zz * point.z
    };
    return out;
}

struct matrix select_array(struct matrix *huchinson_op, double *probabilites,
                           size_t num_op)
{
    double rnd = (double)rand() / RAND_MAX;
    for (size_t i = 0; i < num_op; ++i) {
        if (rnd < probabilites[i]) {
            return huchinson_op[i];
        }
        rnd -= probabilites[i];
    }
}

void chaos_game(struct point2d *output_points, size_t num,
                struct point2d initial_point, struct matrix *huchinson_op,
                double *probabilites, size_t nop)
{
    struct point3d point = {initial_point.x, initial_point.y, 1.0};

    for (size_t i = 0; i < num; ++i) {
        point = matmul(select_array(huchinson_op, probabilites, nop), point);
        output_points[i].x = point.x;
        output_points[i].y = point.y;
    }
}

int main()
{
    struct matrix barnsley_huchinson[4] = {
        {
            0.0, 0.0, 0.0,
            0.0, 0.16, 0.0,
            0.0, 0.0, 1.0
        },
        {
            0.85, 0.04, 0.0,
            -0.04, 0.85, 1.60,
            0.0, 0.0, 1.0
        },
        {
            0.2, -0.26, 0.0,
            0.23, 0.22, 1.60,
            0.0, 0.0, 1.0
        },
        {
            -0.15, 0.28, 0.0,
            0.26, 0.24, 0.44,
            0.0, 0.0, 1.0
        }
    };

    double barnsley_probabilites[4] = {0.01, 0.85, 0.07, 0.07};
    struct point2d output_points[10000];
    struct point2d initial_point = {0.0, 0.0};
    chaos_game(output_points, 10000, initial_point, barnsley_huchinson,
               barnsley_probabilites, 4);
    FILE *f = fopen("barnsley.dat", "w");
    for (size_t i = 0; i < 10000; ++i) {
        fprintf(f, "%f\t%f\n", output_points[i].x, output_points[i].y);
    }
    fclose(f);

    return 0;
}
