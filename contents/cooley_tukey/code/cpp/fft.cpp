// written by Gathros, modernized by Nicole Mazzuca.

#include <algorithm>
#include <array>
#include <complex>
#include <cstdint>
#include <vector>

// These headers are for presentation not for the algorithm.
#include <iomanip>
#include <iostream>
#include <random>

using std::begin;
using std::end;
using std::swap;

using std::size_t;

using complex = std::complex<double>;
static const double pi = 3.14159265358979323846264338327950288419716;

template <typename Iter>
void dft(Iter X, Iter last) {
  const auto N = last - X;
  std::vector<complex> tmp(N);
  for (auto i = 0; i < N; ++i) {
    for (auto j = 0; j < N; ++j) {
      tmp[i] += X[j] * exp(complex(0, -2.0 * M_PI * i * j / N));
    }
  }
  std::copy(std::begin(tmp), std::end(tmp), X);
}

// `cooley_tukey` does the cooley-tukey algorithm, recursively
template <typename Iter>
void cooley_tukey(Iter first, Iter last) {
  auto size = last - first;
  if (size >= 2) {
    // split the range, with even indices going in the first half,
    // and odd indices going in the last half.
    auto temp = std::vector<complex>(size / 2);
    for (int i = 0; i < size / 2; ++i) {
      temp[i] = first[i * 2 + 1];
      first[i] = first[i * 2];
    }
    for (int i = 0; i < size / 2; ++i) {
      first[i + size / 2] = temp[i];
    }

    // recurse the splits and butterflies in each half of the range
    auto split = first + size / 2;
    cooley_tukey(first, split);
    cooley_tukey(split, last);

    // now combine each of those halves with the butterflies
    for (int k = 0; k < size / 2; ++k) {
      auto w = std::exp(complex(0, -2.0 * pi * k / size));

      auto& bottom = first[k];
      auto& top = first[k + size / 2];
      top = bottom - w * top;
      bottom -= top - bottom;
    }
  }
}

// note: (last - first) must be less than 2**32 - 1
template <typename Iter>
void sort_by_bit_reverse(Iter first, Iter last) {
  // sorts the range [first, last) in bit-reversed order,
  // by the method suggested by the FFT
  auto size = last - first;

  for (std::uint32_t i = 0; i < size; ++i) {
    auto b = i;
    b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
    b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
    b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
    b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
    b = ((b >> 16) | (b << 16)) >> (32 - std::uint32_t(log2(size)));
    if (b > i) {
      swap(first[b], first[i]);
    }
  }
}

// `iterative_cooley_tukey` does the cooley-tukey algorithm iteratively
template <typename Iter>
void iterative_cooley_tukey(Iter first, Iter last) {
  sort_by_bit_reverse(first, last);

  // perform the butterfly on the range
  auto size = last - first;
  for (int stride = 2; stride <= size; stride *= 2) {
    auto w = exp(complex(0, -2.0 * pi / stride));
    for (int j = 0; j < size; j += stride) {
      auto v = complex(1.0);
      for (int k = 0; k < stride / 2; k++) {
        first[k + j + stride / 2] =
            first[k + j] - v * first[k + j + stride / 2];
        first[k + j] -= (first[k + j + stride / 2] - first[k + j]);
        v *= w;
      }
    }
  }
}

int main() {
  // initalize the FFT inputs
  std::random_device random_device;
  std::mt19937 rng(random_device());
  std::uniform_real_distribution<double> distribution(0.0, 1.0);

  std::array<complex, 64> initial;
  std::generate(
      begin(initial), end(initial), [&] { return distribution(rng); });

  auto recursive = initial;
  auto iterative = initial;

  // Preform an FFT on the arrays.
  cooley_tukey(begin(recursive), end(recursive));
  iterative_cooley_tukey(begin(iterative), end(iterative));

  // Check if the arrays are approximately equivalent
  std::cout << std::right << std::setw(16) << "idx" << std::setw(16) << "rec"
            << std::setw(16) << "it" << std::setw(16) << "subtracted" << '\n';
  for (size_t i = 0; i < initial.size(); ++i) {
    auto rec = recursive[i];
    auto it = iterative[i];
    std::cout << std::setw(16) << i << std::setw(16) << std::abs(rec)
              << std::setw(16) << std::abs(it) << std::setw(16)
              << (std::abs(rec) - std::abs(it)) << '\n';
  }
}
