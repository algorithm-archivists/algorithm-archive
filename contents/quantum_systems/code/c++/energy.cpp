#include <vector>
#include <complex>

#include <fftw3.h>

void fft(std::vector<std::complex<double>> x, bool inverse) {
    std::vector<std::complex<double>> y(x.size());
    for (size_t i = 0; i < x.size(); i++) {
        y.push_back(std::complex(0.0, 0.0));
    }
    
    fftw_plan p;

    p = fftw_plan_dft_1d(x.size(), reinterpret_cast<fftw_complex*>(x.data()),
                         reinterpret_cast<fftw_complex*>(y.data()),
                         (inverse ? FFTW_BACKWARD : FFTW_FORWARD), FFTW_ESTIMATE);

    fftw_execute(p);
    fftw_destroy_plan(p);

    for (size_t i = 0; i < x.size(); ++i) {
        x[i] = y[i] / sqrt((double)x.size());
    }
}

double calculate_energy(std::vector<std::complex<double>> wfc, std::vector<std::complex<double>> h_r,
                        std::vector<std::complex<double>> h_k, double dx, size_t size) {
    std::vector<std::complex<double>> wfc_k(wfc);
    std::vector<std::complex<double>> wfc_c(size);
    fft(wfc_k, false);

    for (size_t i = 0; i < size; ++i) {
        wfc_c.push_back(conj(wfc[i]));
    }

    std::vector<std::complex<double>> energy_k(size);
    std::vector<std::complex<double>> energy_r(size);

    for (size_t i = 0; i < size; ++i) {
        energy_k.push_back(wfc_k[i] * h_k[i]);
    }

    fft(energy_k, true);

    for (size_t i = 0; i < size; ++i) {
        energy_k[i] *= wfc_c[i];
        energy_r.push_back(wfc_c[i] * h_r[i] * wfc[i]);
    }

    double energy_final = 0;

    for (size_t i = 0; i < size; ++i) {
        energy_final += real(energy_k[i] + energy_r[i]);
    }

    return energy_final * dx;
}
