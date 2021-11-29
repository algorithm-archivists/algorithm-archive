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

// This function reads in the Hutchinson operator and corresponding
// probabilities and returns a randomly selected transform
// This works by choosing a random number and then iterating through all
// probabilities until it finds an appropriate bin
struct matrix select_array(struct matrix *hutchinson_op, double *probabilities,
                           size_t num_op)
{
    // random number to be binned
    double rnd = (double)rand() / RAND_MAX;

    // This checks to see if a random number is in a bin, if not, that
    // probability is subtracted from the random number and we check the next
    // bin in the list
    for (size_t i = 0; i < num_op; ++i) {
        if (rnd < probabilities[i]) {
            return hutchinson_op[i];
        }
        rnd -= probabilities[i];
    }
    return hutchinson_op[0];
}

// This is a general function to simulate a chaos game
//  - output_points: pointer to an initialized output array
//  - num: the number of iterations
//  - initial_point: the starting point of the chaos game
//  - hutchinson_op: the set of functions to iterate through
//  - probabilities: the set of probabilities corresponding to the likelihood
//      of choosing their corresponding function in hutchingson_op
//  - nop: the number of functions in hutchinson_op
void chaos_game(struct point2d *output_points, size_t num,
                struct point2d initial_point, struct matrix *hutchinson_op,
                double *probabilities, size_t nop)
{
    // extending point to 3D for affine transform
    struct point3d point = {initial_point.x, initial_point.y, 1.0};

    for (size_t i = 0; i < num; ++i) {
        point = matmul(select_array(hutchinson_op, probabilities, nop), point);
        output_points[i].x = point.x;
        output_points[i].y = point.y;
    }
}

int main()
{
    struct matrix barnsley_hutchinson[4] = {
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

    double barnsley_probabilities[4] = {0.01, 0.85, 0.07, 0.07};
    struct point2d output_points[10000];
    struct point2d initial_point = {0.0, 0.0};
    chaos_game(output_points, 10000, initial_point, barnsley_hutchinson,
               barnsley_probabilities, 4);
    FILE *f = fopen("barnsley.dat", "w");
    for (size_t i = 0; i < 10000; ++i) {
        fprintf(f, "%f\t%f\n", output_points[i].x, output_points[i].y);
    }
    fclose(f);

    return 0;
}
