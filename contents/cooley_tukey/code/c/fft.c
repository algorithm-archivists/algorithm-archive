#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <fftw3.h>

void fft(double complex *x, int n) {
    double complex y[n];
    memset(y, 0, sizeof(y));
    fftw_plan p;

    p = fftw_plan_dft_1d(n, (fftw_complex*)x, (fftw_complex*)y,
                         FFTW_FORWARD, FFTW_ESTIMATE);

    fftw_execute(p);
    fftw_destroy_plan(p);

    for (size_t i = 0; i < n; ++i) {
        x[i] = y[i] / sqrt((double)n);
    }
}

void dft(double complex *X, const size_t N) {
    double complex tmp[N];
    for (size_t i = 0; i < N; ++i) {
        tmp[i] = 0;
        for (size_t j = 0; j < N; ++j) {
            tmp[i] += X[j] * cexp(-2.0 * M_PI * I * j * i / N);
        }
    }

    memcpy(X, tmp, N * sizeof(*X));
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
            X[i + N / 2] = X[i] - cexp(-2.0 * I * M_PI * i / N) * X[i + N / 2];
            X[i] -= (X[i + N / 2]-X[i]);
        }
    }
}

void bit_reverse(double complex *X, size_t N) {
    for (int i = 0; i < N; ++i) {
        int n = i;
        int a = i;
        int count = (int)log2((double)N) - 1;

        n >>= 1;
        while (n > 0) {
            a = (a << 1) | (n & 1);
            count--;
            n >>= 1;
        }
        n = (a << count) & ((1 << (int)log2((double)N)) - 1);

        if (n > i) {
            double complex tmp = X[i];
            X[i] = X[n];
            X[n] = tmp;
        }
    }
}

void iterative_cooley_tukey(double complex *X, size_t N) {
    bit_reverse(X, N);

    for (int i = 1; i <= log2((double)N); ++i) {
        int stride = pow(2, i);
        double complex w = cexp(-2.0 * I * M_PI / stride);
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
        if (cabs(X[i]) - cabs(Y[i]) > 1E-5) {
            printf("This is not approximate.\n");
            return;
        }
    }
    printf("This is approximate.\n");
}

int main() {
    srand(time(NULL));
    double complex x[64], y[64], z[64];
    for (size_t i = 0; i < 64; ++i) {
        x[i] = rand() / (double) RAND_MAX;
        y[i] = x[i];
        z[i] = x[i];
    }

    fft(x, 64);
    cooley_tukey(y, 64);
    iterative_cooley_tukey(z, 64);

    approx(x, y, 64);
    approx(x, z, 64);

    return 0;
}
