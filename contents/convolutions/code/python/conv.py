import numpy as np
from scipy.fftpack import fft, ifft

def conv(signal1, signal2):
    """Discrete convolution by definition"""
    
    out = []
    n = range(min(len(signal1), len(signal2)))

    for i in n:
        s = 0
        for j in n:
            s += signal1[j] * signal2[i - j]
        out.append(s)

    return out

def conv_fft(signal1, signal2):
    """Convolution using fft and convolutional theorem"""
    
    fft_s1 = fft(signal1)
    fft_s2 = fft(signal2)
    out = []

    for i in range(min(len(signal1), len(signal2))):
        out.append(fft_s1[i] * fft_s2[i])

    return ifft(out)


def main():
    # Example convolution with sin and cos
    x = np.linspace(0, 1, 5)
    s1 = np.sin(x)
    s2 = np.cos(x)

    print("Discrete Convolution")
    print(conv(s1, s2))

    print("FFT Convolution")
    print(conv_fft(s1, s2))


if __name__ == "__main__":
    main()

