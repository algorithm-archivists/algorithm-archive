#include <complex>
#include <iostream>
#include <cstring>

// Using fftw3 library.
#include <fftw3.h>

class Params {
public:
    Params(double _xmax, unsigned int _res, double _dt, unsigned int _timesteps, bool im) {
        xmax = _xmax;
        res = _res;
        dt = _dt;
        timesteps = _timesteps;
        dx = 2.0 * xmax / res;
        x = new double[res];
        dk = M_PI / xmax;
        k = new double[res];
        im_time = im;
        
        for (size_t i = 0; i < res; ++i) {
            x[i] = xmax / res - xmax + i * (2.0 * xmax / res);
            if (i < res / 2) {
                k[i] = i * M_PI / xmax;
            } else {
                k[i] = ((double)i - res) * M_PI / xmax;
            }
        }
    }
    
    ~Params() {
        delete[] x;
        delete[] k;
    }
    
    unsigned int getRes() const {
        return res;
    }
    double getXmax() const {
        return xmax;
    }
    double getDt() const {
        return dt;
    }
    double getDx() const {
        return dx;
    }
    double getDk() const {
        return dk;
    }
    unsigned int getTimesteps() const {
        return timesteps;
    }
    double * getXs() const {
        return x;
    }
    double * getKs() const {
        return k;
    }
    bool isImTime() const {
        return im_time;
    }
    
protected:
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

class Operators {
public:
    Operators(Params &par, double voffset,
              double wfcoffset) {
        size = par.getRes();
        v = new std::complex<double>[size];
        pe = new std::complex<double>[size];
        ke = new std::complex<double>[size];
        wfc = new std::complex<double>[size];
        
        for (size_t i = 0; i < size; ++i) {
            v[i] = 0.5 * pow(par.getXs()[i] - voffset, 2);
            wfc[i] = exp(-pow(par.getXs()[i] - wfcoffset, 2) / 2.0);
            
            if (par.isImTime()) {
                ke[i] = exp(-0.5 * par.getDt() * pow(par.getKs()[i], 2));
                pe[i] = exp(-0.5 * par.getDt() * v[i]);
            } else {
                ke[i] = exp(-0.5 * par.getDt() * pow(par.getKs()[i], 2) * std::complex(0.0, 1.0));
                pe[i] = exp(-0.5 * par.getDt() * v[i] * std::complex(0.0, 1.0));
            }
        }
    }
    
    ~Operators() {
        delete[] v;
        delete[] pe;
        delete[] ke;
        delete[] wfc;
    }
    
    size_t getSize() const {
        return size;
    }
    std::complex<double> *getV() const {
        return v;
    }
    std::complex<double> *getPe() const {
        return pe;
    }
    std::complex<double> *getKe() const {
        return ke;
    }
    std::complex<double> *getWfc() const {
        return wfc;
    }
    
protected:
    size_t size;
    std::complex<double> *v;
    std::complex<double> *pe;
    std::complex<double> *ke;
    std::complex<double> *wfc;
};

void fft(std::complex<double> *x, int n, bool inverse) {
    std::complex<double> y[n];
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

void split_op(Params &par, Operators &opr) {
    double density[opr.getSize()];
    
    for (size_t i = 0; i < par.getTimesteps(); ++i) {
        for (size_t j = 0; j < opr.getSize(); ++j) {
            opr.getWfc()[j] *= opr.getPe()[j];
        }
        
        fft(opr.getWfc(), opr.getSize(), false);
        
        for (size_t j = 0; j < opr.getSize(); ++j) {
            opr.getWfc()[j] *= opr.getKe()[j];
        }
        
        fft(opr.getWfc(), opr.getSize(), true);
        
        for (size_t j = 0; j < opr.getSize(); ++j) {
            opr.getWfc()[j] *= opr.getPe()[j];
        }
        
        for (size_t j = 0; j < opr.getSize(); ++j) {
            density[j] = pow(abs(opr.getWfc()[j]), 2);
        }
        
        if (par.isImTime()) {
            double sum = 0;
            
            for (size_t j = 0; j < opr.getSize(); ++j) {
                sum += density[j];
            }
            
            sum *= par.getDx();
            
            for (size_t j = 0; j < opr.getSize(); ++j) {
                opr.getWfc()[j] /= sqrt(sum);
            }
        }
        
        // Writing data into a file in the format of:
        // index, density, real potential.
        char filename[256];
        sprintf(filename, "output%lu.dat", i);
        FILE *fp = fopen(filename, "w");
        
        for (int i = 0; i < opr.getSize(); ++i) {
            fprintf(fp, "%d\t%f\t%f\n", i, density[i], real(opr.getV()[i]));
        }
        
        fclose(fp);
    }
}

double calculate_energy(Params par, Operators opr) {
    std::complex<double> wfc_r[opr.getSize()];
    std::complex<double> wfc_k[opr.getSize()];
    std::complex<double> wfc_c[opr.getSize()];
    std::memcpy(wfc_r, opr.getWfc(), sizeof(wfc_r));
    
    std::memcpy(wfc_k, opr.getWfc(), sizeof(wfc_k));
    fft(wfc_k, opr.getSize(), false);
    
    for (size_t i = 0; i < opr.getSize(); ++i) {
        wfc_c[i] = conj(wfc_r[i]);
    }
    
    std::complex<double> energy_k[opr.getSize()];
    std::complex<double> energy_r[opr.getSize()];
    
    for (size_t i = 0; i < opr.getSize(); ++i) {
        energy_k[i] = wfc_k[i] * pow(par.getKs()[i] + 0.0*std::complex(0.0, 1.0), 2);
    }
    
    fft(energy_k, opr.getSize(), true);
    
    for (size_t i = 0; i < opr.getSize(); ++i) {
        energy_k[i] *= 0.5 * wfc_c[i];
        energy_r[i] = wfc_c[i] * opr.getV()[i] * wfc_r[i];
    }
    
    double energy_final = 0;
    
    for (size_t i = 0; i < opr.getSize(); ++i) {
        energy_final += real(energy_k[i] + energy_r[i]);
    }
    
    return energy_final * par.getDx();
}

int main() {
    Params par = Params(5.0, 256, 0.05, 100, true);
    Operators opr = Operators(par, 0.0, -1.0);
    
    split_op(par, opr);
    
    printf("The energy is %f\n", calculate_energy(par, opr));
    
    return 0;
}
