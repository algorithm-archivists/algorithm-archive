// written by Gathros.

#include <complex.h>
#include <math.h>

// These headers are for presentation not for the algorithm.
#include <stdlib.h>
#include <time.h>
#include <stdio.h>

#define PI 3.1415926535897932384626

void cooley_tukey(double complex *X, const size_t N){
	if(N >= 2){
		// Splits the array, so the top half are the odd elements and the bottom half are the even ones.
		double complex tmp [N/2];
		for(size_t i = 0; i < N/2; ++i){
			tmp[i] = X[2*i + 1];
			X[i] = X[2*i];
		}
		for(size_t i = 0; i < N/2; ++i){
			X[i + N/2] = tmp[i];
		}

		// Recursion.
		cooley_tukey(X, N/2);
		cooley_tukey(X + N/2, N/2);

		// Combine.
		for(size_t i = 0; i < N/2; ++i){
			X[i + N/2] = X[i] - cexp(-2.0*I*PI*i/N)*X[i + N/2];
			X[i] -= (X[i + N/2]-X[i]);
		}
	}
}

void bit_reverse(double complex *X, size_t N){
        // Bit reverses the array X[] but only if the size of the array is less then 2^32.
		double complex temp;
        unsigned int b;

        for(unsigned int i = 0; i < N; ++i){
                b = i;
                b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
                b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
                b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
                b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
                b = ((b >> 16) | (b << 16)) >> (32 - (unsigned int) log2((double)N));
                if(b > i){
                        temp = X[b];
                        X[b] = X[i];
                        X[i] = temp;
                }
        }
}

void iterative_cooley_tukey(double complex *X, size_t N){
	int stride;
	double complex v,w;

	// Bit reverse the array.
	bit_reverse(X, N);

	// Preform the butterfly on the array.
	for(int i = 1; i <= log2((double)N); ++i){
		stride = pow(2, i);
		w = cexp(-2.0*I*PI/stride);
		for(size_t j = 0; j < N; j += stride){
			v = 1.0;
			for(size_t k = 0; k < stride/2; k++){
				X[k + j + stride/2] = X[k + j] - v*X[k + j + stride/2];
				X[k + j] -= (X[k + j + stride/2] - X[k + j]);
				v *= w;
			}
		}
	}
}

void approx(double complex *X, double complex *Y, size_t N){
	// This is to show that the arrays are approximate.
	for(size_t i = 0; i < N; ++i){
		printf("%f\n", cabs(X[i]) - cabs(Y[i]));
	}
}

int main(){
	// Initalizing the arrays for FFT.
	srand(time(NULL));
	const size_t N = 64;
	double complex x[N], y[N], z[N];
	for(size_t i = 0; i < N; ++i){
		x[i] = rand() / (double) RAND_MAX;
		y[i] = x[i];
		z[i] = x[i];
	}

	// Preform FFT.
	cooley_tukey(y, N);
	iterative_cooley_tukey(z, N);

	// Check if the different methods are approximate.
	approx(y, z, N);

	return 0;
}
