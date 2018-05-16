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

using c64 = std::complex<double>;
template <typename T>
constexpr T pi() {
  return 3.14159265358979323846264338327950288419716;
}

template <typename Iter>
void dft(Iter first, Iter last) {
  auto size = last - first;
  auto temp = std::vector<c64>(size);

  for (size_t i = 0; i < size; ++i) {
    temp[i] = 0;
    for (size_t j = 0; j < size; ++j) {
      temp[i] +=
          first[j] * std::exp(c64(0, -2.0 * pi<double>() * j * i / size));
    }
  }

  for (size_t i = 0; i < size; ++i) {
    first[i] = temp[i];
  }
}

// `cooley_tukey` does the cooley-tukey algorithm, recursively
template <typename Iter>
void cooley_tukey(Iter first, Iter last) {
  auto size = last - first;
  if (size >= 2) {
    // split the range, with even indices going in the first half,
    // and odd indices going in the last half.
    auto temp = std::vector<c64>(size / 2);
    for (size_t i = 0; i < size / 2; ++i) {
      temp[i] = first[i * 2 + 1];
      first[i] = first[i * 2];
    }
    for (size_t i = 0; i < size / 2; ++i) {
      first[i + size / 2] = temp[i];
    }

    // recurse the splits and butterflies in each half of the range
    auto split = first + size / 2;
    cooley_tukey(first, split);
    cooley_tukey(split, last);

    // now combine each of those halves with the butterflies
    for (size_t k = 0; k < size / 2; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double>() * k / size));

      auto& bottom = first[k];
      auto& top = first[k + size / 2];
      top = bottom - w * top;
      bottom -= top - bottom;
    }
  }
}

template <typename Iter>
void sort_by_bit_reverse(Iter first, Iter last) {
  // sorts the range [first, last) in bit-reversed order,
  // by the method suggested by the FFT

  for (int i = 0; i < last - first; ++i) {
    int n = i;
    int a = i;
    int count = static_cast<int>(std::log2(last - first)) - 1;

    n >>= 1;
    while (n > 0) {
      a = (a << 1) | (n & 1);
      count--;
      n >>= 1;
    }
    n = (a << count) & ((1 << static_cast<int>(std::log2(last - first))) - 1);

    if (n > i) {
      swap(first[i], first[n]);
    }
  }
}

// `iterative_cooley_tukey` does the cooley-tukey algorithm iteratively
template <typename Iter>
void iterative_cooley_tukey(Iter first, Iter last) {
  sort_by_bit_reverse(first, last);

  // perform the butterfly on the range
  auto size = last - first;
  for (size_t stride = 2; stride <= size; stride *= 2) {
    auto w = exp(c64(0, -2.0 * pi<double>() / stride));
    for (size_t j = 0; j < size; j += stride) {
      auto v = c64(1.0);
      for (size_t k = 0; k < stride / 2; k++) {
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

  std::array<c64, 64> initial;
  std::generate(
      begin(initial), end(initial), [&] { return distribution(rng); });

  auto recursive = initial;
  auto iterative = initial;

  // Preform an FFT on the arrays.
  cooley_tukey(begin(recursive), end(recursive));
  dft(begin(iterative), end(iterative));

  // Check if the arrays are approximately equivalent
  std::cout << std::right << std::setw(16) << "idx" << std::setw(16) << "rec"
            << std::setw(16) << "it" << std::setw(16) << "subtracted" << '\n';
  for (int i = 0; i < initial.size(); ++i) {
    auto rec = recursive[i];
    auto it = iterative[i];
    std::cout << std::setw(16) << i << std::setw(16) << std::abs(rec)
              << std::setw(16) << std::abs(it) << std::setw(16)
              << (std::abs(rec) - std::abs(it)) << '\n';
  }
}
