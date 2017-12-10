#include <complex>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <iostream>

void cooley_tukey(std::complex<double> *X, int N){
    if(N >= 2){
        std::complex<double> *temp = new std::complex<double>[N/2];
    	for(int i=0; i< N/2; i++){
        	temp[i] = X[i*2 + 1];
        	X[i] = X[i*2];
		}
    	for(int i=0; i< N/2; i++){
        	X[i + N/2] = temp[i];
		}
    	delete[] temp;

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
	std::complex<double> *temp = new std::complex<double>[N];
	unsigned int num, nrev, bnum;

	for(unsigned int i = 0; i < N; ++i){
    	bnum = 1<<(unsigned int)log2(N);
    	nrev = num = i;

    	for(int j = 1; j<log2(N); ++j){
        	num >>= 1;
        	nrev <<= 1;
        	nrev |= num & 1;
    	}
    	nrev &= bnum-1;
		temp[i] = X[(int)nrev];
	}

	for(int i = 0; i < N; ++i){
		X[i] = temp[i];
	}
	delete[] temp;
}

void iterative_cooley_tukey(std::complex<double> *X, int N){
	int stride;
	std::complex<double> u,t;

	bitReverse(X, N);
	for(int i = 1; i < log2(N); ++i){
		stride = std::pow(2, i);
		for(int j = 0; j < N; j += stride){
			for(int k = 0; k < stride/2; k++){
				u = X[k] + X[k + stride/2]*exp(std::complex<double>(0, -2.0*M_PI*k/stride));
				t = X[k] - X[k + stride/2]*exp(std::complex<double>(0, -2.0*M_PI*k/stride));
				X[k] = u;
				X[k + stride/2] = t;
			}
		}
	}
}

void approx(std::complex<double> *X, std::complex<double> *Y, int N){
	for(int i = 0; i < N; ++i){
		std::cout << std::abs(X[i]) - std::abs(Y[i]) << std::endl;
	}
}

int main(){
	std::cout << "a" << std::endl;
	srand(time(NULL));
	std::complex<double> x[64], y[64], z[64];
	std::cout << "ab" << std::endl;
	for(int i = 0; i < 64; ++i){
		x[i] = y[i] = z[i] = std::complex<double>(rand() / (double) RAND_MAX, 0.0);
	}

	std::cout << "abc" << std::endl;
	cooley_tukey(y, 64);
	std::cout << "abcd" << std::endl;
	iterative_cooley_tukey(z, 64);

	approx(y,z,64);
	return 0;
}
