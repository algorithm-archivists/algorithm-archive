#include <array>
#include <complex>
#include <cstdint>
#include <vector>

// These headers are for presentation not for the algorithm.
#include <iomanip>
#include <iostream>
#include <random>

using c64 = std::complex<double>;
template <typename T>
constexpr T pi() {
  return 3.14159265358979323846264338327950288419716;
}

// This section is not a part of the algorithm

template <typename Iter, typename Iter_end>
void fft(Iter start, Iter_end end) {
  auto size = end - start;
  if (size >= 2) {
    // Splits the array, so the top half are the odd elements and the bottom are
    // the even ones.
    auto temp = std::vector<c64>(size / 2);
    for (std::size_t i = 0; i < size / 2; ++i) {
      temp[i] = start[i * 2 + 1];
      start[i] = start[i * 2];
    }
    for (std::size_t i = 0; i < size / 2; ++i) {
      start[i + size / 2] = temp[i];
    }

    // Recursion.
    fft(start, start + size / 2);
    fft(start + size / 2, end);

    // Combine.
    for (std::size_t k = 0; k < size / 2; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double>() * k / size));
      start[k + size / 2] = start[k] - w * start[k + size / 2];
      start[k] -= (start[k + size / 2] - start[k]);
    }
  }
}

template <typename Iter, typename Iter_end>
void ifft(Iter start, Iter_end end) {
  auto size = end - start;
  for (std::size_t i = 0; i < size; i++) {
    start[i] = std::conj(start[i]);
  }

  fft(start, end);

  for (std::size_t i = 0; i < size; i++) {
    start[i] = std::conj(start[i]) / static_cast<c64>(size);
  }
}

// This section is a part of the algorithm

template <typename Iter, typename Iter_end>
void conv(
    Iter signal1start,
    Iter_end signal1end,
    Iter signal2start,
    Iter_end signal2end,
    Iter outstart,
    Iter_end outend) {

  auto size1 = (signal1end - signal1start);
  auto size2 = (signal2end - signal2start);
  c64 sum = 0;

  for (std::size_t i = 0; i < (size1 < size2 ? size2 : size1); i++) {
    for (std::size_t j = 0; j < i; j++) {
      if (j < size1) {
        sum += signal1start[j] * signal2start[i - j];
      }
    }
    outstart[i] = sum;
    sum = 0;
  }
}

template <typename Iter, typename Iter_end>
void conv_fft(
    Iter signal1start,
    Iter_end signal1end,
    Iter signal2start,
    Iter_end signal2end,
    Iter outstart,
    Iter_end outend) {
  fft(signal1start, signal1end);
  fft(signal2start, signal2end);

  for (std::size_t i = 0; i < (signal1end - signal1start); i++) {
    outstart[i] = signal1start[i] * signal2start[i];
  }

  ifft(outstart, outend);
}

int main() {
  std::array<c64, 64> signal1, signal2;
  std::array<c64, 128> signal3, signal4, out1, out2;

  for (std::size_t i = 0; i < 128; i++) {
    if (i >= 16 && i < 48) {
      signal1[i] = 1.0;
      signal2[i] = 1.0;
      signal3[i] = 1.0;
      signal4[i] = 1.0;
      out1[i] = 0.0;
      out2[i] = 0.0;
    } else if (i >= 64) {
      signal3[i] = 0.0;
      signal4[i] = 0.0;
      out1[i] = 0.0;
      out2[i] = 0.0;
    } else {
      signal1[i] = 0.0;
      signal2[i] = 0.0;
      signal3[i] = 0.0;
      signal4[i] = 0.0;
      out1[i] = 0.0;
      out2[i] = 0.0;
    }
  }

  conv(
      signal1.begin(),
      signal1.end(),
      signal2.begin(),
      signal2.end(),
      out1.begin(),
      out1.end());
  conv_fft(
      signal3.begin(),
      signal3.end(),
      signal4.begin(),
      signal4.end(),
      out2.begin(),
      out2.end());

  std::cout << std::right << std::setw(16) << "i" << std::setw(16)
            << "subtracted" << '\n';
  for (int i = 0; i < signal1.size(); ++i) {
    std::cout << std::setw(16) << i << std::setw(16)
              << (std::abs(out1[i]) - std::abs(out2[i])) << '\n';
  }

  return 0;
}
