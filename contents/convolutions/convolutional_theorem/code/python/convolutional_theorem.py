from scipy.fft import fft, ifft
import numpy as np

# using the convolutional theorem
def convolve_fft(signal1, signal2):
    return ifft(np.multiply(fft(signal1),fft(signal2)))

# Sawtooth functions
x = [float(i)/200 for i in range(1,101)]
y = [float(i)/200 for i in range(1,101)]

x /= np.linalg.norm(x)
y /= np.linalg.norm(y)

# Convolving the two signals
fft_output = convolve_fft(x, y)

np.savetxt("fft.dat", np.real(fft_output))

