from random import random
from cmath import exp, pi
from math import log2

def dft(X):
    N = len(X)
    temp = [0]*N
    for i in range(N):
        for k in range(N):
            temp[i] += X[k] * exp(-2.0j*pi*i*k/N)
    return temp

def cooley_tukey(X):
	N = len(X)
	if N <= 1:
		return X
	even = cooley_tukey(X[0::2])
	odd =  cooley_tukey(X[1::2])

	temp = [i for i in range(N)]
	for k in range(N//2):
		temp[k] = even[k] + exp(-2j*pi*k/N) * odd[k]
		temp[k+N//2] = even[k] - exp(-2j*pi*k/N)*odd[k]
	return temp

def bitReverse(X):
	N = len(X)
	temp = [i for i in range(N)]
	for k in range(N):
		b =  sum(1<<(int(log2(N))-1-i) for i in range(int(log2(N))) if k>>i&1)
		temp[k] = X[b]
		temp[b] = X[k]
	return temp

def iterative_cooley_tukey(X):
	N = len(X)

	X = bitReverse(X)

	for i in range(1, int(log2(N)) + 1):
		stride = 2**i
		w = exp(-2j*pi/stride)
		for j in range(0, N, stride):
			v = 1
			for k in range(stride//2):
				X[k + j + stride//2] = X[k + j] - v*X[k + j + stride//2];
				X[k + j] -= (X[k + j + stride//2] - X[k + j]);
				v *= w;
	return X

X = []

for i in range(64):
	X.append(random())

Y = cooley_tukey(X)
Z = iterative_cooley_tukey(X)
T = dft(X)

print(all(abs([Y[i] - Z[i] for i in range(64)][j]) < 1 for j in range(64)))
print(all(abs([Y[i] - T[i] for i in range(64)][j]) < 1 for j in range(64)))
