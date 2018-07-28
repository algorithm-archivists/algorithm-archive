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

double calculate_energy(struct params par, struct operators opr) {
    double complex wfc_r[opr.size];
    double complex wfc_k[opr.size];
    double complex wfc_c[opr.size];
    memcpy(wfc_r, opr.wfc, sizeof(wfc_r));

    memcpy(wfc_k, opr.wfc, sizeof(wfc_k));
    fft(wfc_k, opr.size, false);

    for (size_t i = 0; i < opr.size; ++i) {
        wfc_c[i] = conj(wfc_r[i]);
    }

    double complex energy_k[opr.size];
    double complex energy_r[opr.size];

    for (size_t i = 0; i < opr.size; ++i) {
        energy_k[i] = wfc_k[i] * cpow(par.k[i] + 0.0*I, 2);
    }

    fft(energy_k, opr.size, true);

    for (size_t i = 0; i < opr.size; ++i) {
        energy_k[i] *= 0.5 * wfc_c[i];
        energy_r[i] = wfc_c[i] * opr.v[i] * wfc_r[i];
    }

    double energy_final = 0;

    for (size_t i = 0; i < opr.size; ++i) {
        energy_final += creal(energy_k[i] + energy_r[i]);
    }

    return energy_final * par.dx;
}
