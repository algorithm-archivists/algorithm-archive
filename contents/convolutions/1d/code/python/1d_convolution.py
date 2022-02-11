import numpy as np

def mod1(x, y): return ((x % y) + y) % y

def convolve_cyclic(signal, filter_array):
    output_size = max(len(signal), len(filter_array))
    out = np.zeros(output_size)
    s = 0

    for i in range(output_size):
        for j in range(output_size):
            if(mod1(i - j, output_size) < len(filter_array)):
                s += signal[mod1(j - 1, output_size)] * filter_array[mod1(i - j, output_size)]
        out[i] = s
        s = 0

    return out


def convolve_linear(signal, filter_array, output_size):
    out = np.zeros(output_size)
    s = 0

    for i in range(output_size):
        for j in range(max(0, i - len(filter_array)), i + 1):
            if j < len(signal) and (i - j) < len(filter_array):
                s += signal[j] * filter_array[i - j]
        out[i] = s
        s = 0

    return out

# sawtooth functions for x and y
x = [float(i + 1)/200 for i in range(200)]
y = [float(i + 1)/200 for i in range(200)]

# Normalization is not strictly necessary, but good practice
x /= np.linalg.norm(x)
y /= np.linalg.norm(y)

# full convolution, output will be the size of x + y - 1
full_linear_output = convolve_linear(x, y, len(x) + len(y) - 1)

# simple boundaries
simple_linear_output = convolve_linear(x, y, len(x))

# cyclic convolution
cyclic_output = convolve_cyclic(x, y)

# outputting convolutions to different files for plotting in external code
np.savetxt('full_linear.dat', full_linear_output)
np.savetxt('simple_linear.dat', simple_linear_output)
np.savetxt('cyclic.dat', cyclic_output)
