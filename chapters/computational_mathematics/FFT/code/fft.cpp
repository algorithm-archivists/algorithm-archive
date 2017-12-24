#include <complex>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <iostream>

void cooley_tukey(std::complex<double> *X, int N){
    if(N >= 2){
        std::complex<double> temp[N/2];
    	for(int i=0; i< N/2; i++){
        	temp[i] = X[i*2 + 1];
        	X[i] = X[i*2];
	}
    	for(int i=0; i< N/2; i++){
        	X[i + N/2] = temp[i];
	}

        cooley_tukey(X, N/2);
        cooley_tukey(X + N/2, N/2);

	std::complex<double> w;
        for(int k = 0; k < N/2; k++){
            w = exp(std::complex<double>(0, -2.0*M_PI*k/N));
            X[k + N/2] = X[k] - w*X[k + N/2];
            X[k] -= (X[k + N/2]-X[k]);
        }
    }
}

void bitReverse(std::complex<double> *X, int N){
        std::complex<double> temp;
        unsigned int b;

        for(unsigned int i = 0; i < N; ++i){
                b = i;
                // Reverse bits
                b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
                b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
                b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
                b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
                b = ((b >> 16) | (b << 16)) >> (32 - (unsigned int) log2(N));
                if(b > i){
                        temp = X[b];
                        X[b] = X[i];
                        X[i] = temp;
                }
        }
}

void iterative_cooley_tukey(std::complex<double> *X, int N){
	int stride;
	std::complex<double> v,w;

	bitReverse(X, N);

	for(int i = 1; i <= log2(N); ++i){
		stride = std::pow(2, i);
		w = exp(std::complex<double>(0, -2.0*M_PI/stride));
		for(int j = 0; j < N; j += stride){
			v = 1.0;
			for(int k = 0; k < stride/2; k++){
				X[k + j + stride/2] = X[k + j] - v*X[k + j + stride/2];
            			X[k + j] -= (X[k + j + stride/2]-X[k + j]);
				v *= w;
			}
		}
	}
}

int main(){
	srand(time(NULL));
	std::complex<double> x[64], y[64], z[64];
	for(int i = 0; i < 64; ++i){
		x[i] = y[i] = z[i] = std::complex<double>(rand() / (double) RAND_MAX, 0.0);
	}

	cooley_tukey(y, 64);
	iterative_cooley_tukey(z, 64);

	for(int i = 0; i < 64; ++i){
		std::cout << i << "\t" << std::abs(y[i]) - std::abs(z[i]) << std::endl;
	}
	return 0;
}
