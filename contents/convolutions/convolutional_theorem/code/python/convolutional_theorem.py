from scipy.fft import fft, ifft
import numpy as np

# using the convolutional theorem
def convolve_fft(signal1, signal2):
    return ifft(np.multiply(fft(signal1),fft(signal2)))

# Random distribution in x
x = np.random.rand(100)

# Gaussian signals
y = [np.exp(-((i-50)/100)**2/.01) for i in range(1,101)]

x /= np.linalg.norm(x)
y /= np.linalg.norm(y)

# Convolving the two signals
fft_output = convolve_fft(x, y)

np.savetxt("fft.dat", fft_output)

