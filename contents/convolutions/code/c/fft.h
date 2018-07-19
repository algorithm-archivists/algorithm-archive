#ifndef FFT_H
#define FFT_H

#include <complex.h>
#include <math.h>

void fft(double complex *X, size_t N) {
    if (N >= 2) {
        double complex tmp [N / 2];
        for (size_t i = 0; i < N / 2; ++i) {
            tmp[i] = X[2 * i + 1];
            X[i] = X[2 * i];
        }

        for (size_t i = 0; i < N / 2; ++i) {
            X[i + N / 2] = tmp[i];
        }

        fft(X, N / 2);
        fft(X + N / 2, N / 2);

        for (size_t i = 0; i < N / 2; ++i) {
            X[i + N/2] = X[i] - cexp(-2.0 * I * M_PI * i / N) * X[i + N / 2];
            X[i] -= (X[i + N / 2] - X[i]);
        }
    }
}

void ifft(double complex *x, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        x[i] = conj(x[i]);
    }

    fft(x, n);

    for (size_t i = 0; i < n; ++i) {
        x[i] = conj(x[i]) / n;
    }
}

#endif //FFT_H
