from scipy.fft import fft, ifft
import numpy as np

def convolve_fft(signal1, signal2):
    return ifft(np.multiply(fft(signal1),fft(signal2)))


x = np.random.rand(100)
y = [np.exp(-((i-50)/100)**2/.01) for i in range(1,101)]

x /= np.linalg.norm(x)
y /= np.linalg.norm(y)

fft_output = convolve_fft(x, y)
np.savetxt("fft.dat", fft_output)

