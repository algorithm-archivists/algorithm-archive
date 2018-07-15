#include "fft.h"

#include <stdio.h>

void conv(double complex *signal1, double complex *signal2, double complex* out,
          size_t n1, size_t n2) {
    double complex sum = 0;

    for (size_t i = 0; i < (n1 < n2? n2 : n1); ++i) {
        for (size_t j = 0; j < i; ++j) {
            if (j < n1) {
                sum += signal1[j] * signal2[i-j];
            }
        }
        out[i] = sum;
        sum = 0;
    }
}

void conv_fft(double complex *signal1, double complex *signal2,
              double complex* out, size_t n) {
    fft(signal1, n);
    fft(signal2, n);

    for (size_t i = 0; i < n; ++i) {
        out[i] = signal1[i] * signal2[i];
    }

    ifft(out, n);
}

int main() {
    double complex signal1[64], signal2[64], signal3[128], signal4[128],
                    out1[128], out2[128];

    for (size_t i = 0; i < 128; ++i) {
        if (i >= 16 && i < 48) {
            signal1[i] = 1.0;
            signal2[i] = 1.0;
            signal3[i] = 1.0;
            signal4[i] = 1.0;
            out1[i] = 0.0;
            out2[i] = 0.0;
        } else if (i >= 64) {
            signal3[i] = 0.0;
            signal4[i] = 0.0;
            out1[i] = 0.0;
            out2[i] = 0.0;
        } else {
            signal1[i] = 0.0;
            signal2[i] = 0.0;
            signal3[i] = 0.0;
            signal4[i] = 0.0;
            out1[i] = 0.0;
            out2[i] = 0.0;
        }
    }

    conv(signal1, signal2, out1, 64, 64);
    conv_fft(signal3, signal4, out2, 128);

    for (size_t i = 0; i < 64; ++i) {
        printf("%zu %f %+fi\n", i, creal(out1[i]) - creal(out2[i]),
                cimag(out1[i]) - cimag(out2[i]));
    }

    return 0;
}
