import math
from scipy.fftpack import fft, ifft

def conv(signal1, signal2):
    """Discrete convolution by definition"""

    n = len(signal1) + len(signal2)
    signal1 = signal1.copy()
    signal2 = signal2.copy()

    # pad signals to same len
    max_len = max(len(signal1), len(signal2))
    
    for i in range(max_len - len(signal1)):
        signal1.append(0)
    for i in range(max_len - len(signal2)):
        signal2.append(0)
        
    # build output signal
    out = []
    
    for i in range(n):
        s = complex(0)
        
        for j in range(i):
            if j < len(signal1) and i - j - 1  < len(signal2):
                s += signal1[j] * signal2[i - j - 1]
                
        out.append(s)

    return out[1:]


def conv_fft(signal1, signal2):
    """Convolution using fft and convolutional theorem"""
    
    fft_s1 = fft(signal1)
    fft_s2 = fft(signal2)
    out = []

    for i in range(min(len(signal1), len(signal2))):
        out.append(fft_s1[i] * fft_s2[i])

    return list(ifft(out))


def main():
    # Example convolution with sin and cos
    s1 = [math.sin(x) for x in range(5)]
    s2 = [math.cos(x) for x in range(5)]

    print("Discrete Convolution")
    print(conv(s1, s2))

    print("FFT Convolution")
    print(conv_fft(s1, s2))


if __name__ == "__main__":
    main()

