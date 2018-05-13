#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define PI 3.1415926535897932384626

void dft(double complex *X, const size_t N) {
    double complex tmp[N];
    for (size_t i = 0; i < N; ++i) {
        double complex sum = 0 + 0 * I;
        for (size_t j = 0; j < N; ++j) {
            sum += X[j] * cexp(-2.0 * PI * I * j * i / N);
        }

        tmp[i] = sum;
    }

    for (size_t i = 0; i < N; ++i) {
        X[i] = tmp[i];
    }
}

void cooley_tukey(double complex *X, const size_t N) {
    if (N >= 2) {
        double complex tmp [N / 2];
        for (size_t i = 0; i < N / 2; ++i) {
            tmp[i] = X[2*i + 1];
            X[i] = X[2*i];
        }
        for (size_t i = 0; i < N / 2; ++i) {
            X[i + N / 2] = tmp[i];
        }

        cooley_tukey(X, N / 2);
        cooley_tukey(X + N / 2, N / 2);

        for (size_t i = 0; i < N / 2; ++i) {
            X[i + N / 2] = X[i] - cexp(-2.0 * I * PI * i / N) * X[i + N / 2];
            X[i] -= (X[i + N / 2]-X[i]);
        }
    }
}

void bit_reverse(double complex *X, size_t N) {
    double complex temp;
    unsigned int b;

    for (unsigned int i = 0; i < N; ++i) {
        b = i;
        b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
        b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
        b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
        b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
        b = ((b >> 16) | (b << 16)) >> (32 - (unsigned int) log2((double)N));
        if (b > i) {
            temp = X[b];
            X[b] = X[i];
            X[i] = temp;
        }
    }
}

void iterative_cooley_tukey(double complex *X, size_t N) {
    bit_reverse(X, N);

    for (int i = 1; i <= log2((double)N); ++i) {
        int stride = pow(2, i);
        double complex w = cexp(-2.0 * I * PI / stride);
        for (size_t j = 0; j < N; j += stride) {
            double complex v = 1.0;
            for (size_t k = 0; k < stride / 2; ++k) {
                X[k + j + stride / 2] = X[k + j] - v * X[k + j + stride / 2];
                X[k + j] -= (X[k + j + stride / 2] - X[k + j]);
                v *= w;
            }
        }
    }
}

void approx(double complex *X, double complex *Y, size_t N) {
    for (size_t i = 0; i < N; ++i) {
        printf("%f\n", cabs(X[i]) - cabs(Y[i]));
    }
}

int main() {
    srand(time(NULL));
    double complex x[64], y[64], z[64];
    for (size_t i = 0; i < 64; ++i) {
        x[i] = rand() / (double) RAND_MAX;
        y[i] = x[i];
        z[i] = x[i];
    }

    cooley_tukey(y, 64);
    //iterative_cooley_tukey(z, 64);
    dft(z, 64);

    approx(y, z, 64);

    return 0;
}
