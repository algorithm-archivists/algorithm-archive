#include <complex.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Using fftw3 library.
#include <fftw3.h>

struct params {
    double xmax;
    unsigned int res;
    double dt;
    unsigned int timesteps;
    double dx;
    double *x;
    double dk;
    double *k;
    bool im_time;
};

struct operators {
    size_t size;
    double complex *v;
    double complex *pe;
    double complex *ke;
    double complex *wfc;
};

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

void init_params(struct params *par, double xmax, unsigned int res, double dt,
                 unsigned int timesteps, bool im) {

    par->xmax = xmax;
    par->res = res;
    par->dt = dt;
    par->timesteps = timesteps;
    par->dx = 2.0 * xmax / res;
    par->x = malloc(sizeof(double) * res);
    par->dk = M_PI / xmax;
    par->k = malloc(sizeof(double) * res);
    par->im_time = im;

    for (size_t i = 0; i < res; ++i) {
        par->x[i] = xmax / res - xmax + i * (2.0 * xmax / res);
        if (i < res / 2) {
            par->k[i] = i * M_PI / xmax;
        } else {
            par->k[i] = ((double)i - res) * M_PI / xmax;
        }
    }
}

void init_operators(struct operators *opr, struct params par, double voffset,
                    double wfcoffset) {

    opr->size = par.res;
    opr->v = malloc(sizeof(double complex) * par.res);
    opr->pe = malloc(sizeof(double complex) * par.res);
    opr->ke = malloc(sizeof(double complex) * par.res);
    opr->wfc = malloc(sizeof(double complex) * par.res);

    for (size_t i = 0; i < par.res; ++i) {
        opr->v[i] = 0.5 * cpow(par.x[i] - voffset, 2);
        opr->wfc[i] = cexp(-cpow(par.x[i] - wfcoffset, 2) / 2.0);

        if (par.im_time) {
            opr->ke[i] = cexp(-0.5 * par.dt * cpow(par.k[i], 2));
            opr->pe[i] = cexp(-0.5 * par.dt * opr->v[i]);
        } else {
            opr->ke[i] = cexp(-0.5 * par.dt * cpow(par.k[i], 2) * I);
            opr->pe[i] = cexp(-0.5 * par.dt * opr->v[i] * I);
        }
    }
}

void split_op(struct params par, struct operators opr) {
    double density[opr.size];

    for (size_t i = 0; i < par.timesteps; ++i) {
        for (size_t j = 0; j < opr.size; ++j) {
            opr.wfc[j] *= opr.pe[j];
        }

        fft(opr.wfc, opr.size, false);

        for (size_t j = 0; j < opr.size; ++j) {
            opr.wfc[j] *= opr.ke[j];
        }

        fft(opr.wfc, opr.size, true);

        for (size_t j = 0; j < opr.size; ++j) {
            opr.wfc[j] *= opr.pe[j];
        }

        for (size_t j = 0; j < opr.size; ++j) {
            density[j] = pow(cabs(opr.wfc[j]), 2);
        }

        if (par.im_time) {
            double sum = 0;

            for (size_t j = 0; j < opr.size; ++j) {
                sum += density[j];
            }

            sum *= par.dx;

            for (size_t j = 0; j < opr.size; ++j) {
                opr.wfc[j] /= sqrt(sum);
            }
        }

        // Writing data into a file in the format of:
        // index, density, real potential.
        char filename[256];
        sprintf(filename, "output%lu.dat", i);
        FILE *fp = fopen(filename, "w");

        for (int i = 0; i < opr.size; ++i) {
            fprintf(fp, "%d\t%f\t%f\n", i, density[i], creal(opr.v[i]));
        }

        fclose(fp);
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

void free_params(struct params par) {
    free(par.x);
    free(par.k);
}

void free_operators(struct operators opr) {
    free(opr.v);
    free(opr.pe);
    free(opr.ke);
    free(opr.wfc);
}

int main() {
    struct params par;
    struct operators opr;

    init_params(&par, 5.0, 256, 0.05, 100, true);
    init_operators(&opr, par, 0.0, -1.0);

    split_op(par, opr);

    printf("the energy is %f\n", calculate_energy(par, opr));

    free_params(par);
    free_operators(opr);

    return 0;
}
