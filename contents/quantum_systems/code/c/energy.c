#include <stdbool.h>
#include <complex.h>
#include <string.h>
#include <math.h>

#include <fftw3.h>

void fft(double complex *x, int n, bool inverse) {
    double complex y[n];
    memset(y, 0, sizeof(y));
    fftw_plan p;

    if (inverse) {
        p = fftw_plan_dft_1d(n, (fftw_complex*)x, (fftw_complex*)y,
                             FFTW_BACKWARD, FFTW_ESTIMATE);
    } else {
        p = fftw_plan_dft_1d(n, (fftw_complex*)x, (fftw_complex*)y,
                             FFTW_FORWARD, FFTW_ESTIMATE);
    }

    fftw_execute(p);
    fftw_destroy_plan(p);

    for (size_t i = 0; i < n; ++i) {
        x[i] = y[i] / sqrt((double)n);
    }
}

double calculate_energy(double complex *wfc, double complex *h_r,
                        double complex *h_k, double dx, size_t size) {
    double complex wfc_k[size];
    double complex wfc_c[size];
    memcpy(wfc_k, wfc, sizeof(wfc_k));
    fft(wfc_k, size, false);

    for (size_t i = 0; i < size; ++i) {
        wfc_c[i] = conj(wfc[i]);
    }

    double complex energy_k[size];
    double complex energy_r[size];

    for (size_t i = 0; i < size; ++i) {
        energy_k[i] = wfc_k[i] * h_k[i];
    }

    fft(energy_k, size, true);

    for (size_t i = 0; i < size; ++i) {
        energy_k[i] *= wfc_c[i];
        energy_r[i] = wfc_c[i] * h_r[i] * wfc[i];
    }

    double energy_final = 0;

    for (size_t i = 0; i < size; ++i) {
        energy_final += creal(energy_k[i] + energy_r[i]);
    }

    return energy_final * dx;
}
