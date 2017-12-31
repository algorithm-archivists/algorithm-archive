// written by Gathros, modernized by Nicole Mazzuca.

#include <complex>
#include <vector>
#include <array>
#include <cstdint>

// These headers are for presentation not for the algorithm.
#include <random>
#include <iostream>
#include <iomanip>

using c64 = std::complex<double>;
template <typename T>
constexpr T pi() {
  return 3.14159265358979323846264338327950288419716;
}

template <typename Iter, typename Iter_end>
void cooley_tukey(Iter start, Iter_end end) {
  auto size = end - start;
  if (size >= 2) {
    // Splits the array, so the top half are the odd elements and the bottom are the even ones.
    auto temp = std::vector<c64>(size / 2);
    for (std::size_t i = 0; i < size / 2; ++i) {
      temp[i] = start[i * 2 + 1];
      start[i] = start[i * 2];
    }
    for (std::size_t i = 0; i < size / 2; ++i) {
      start[i + size / 2] = temp[i];
    }

    // Recursion.
    cooley_tukey(start, start + size / 2);
    cooley_tukey(start + size / 2, end);

    // Combine.
    for (std::size_t k = 0; k < size / 2; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double>() * k / size));
      start[k + size / 2] = start[k] - w * start[k + size / 2];
      start[k] -= (start[k + size / 2] - start[k]);
    }
  }
}


template <typename Iter, typename Iter_end>
void bit_reverse(Iter start, Iter_end end) {
  // Bit reverses the array X[] but only if the size of the array is less then 2^32.
  auto size = end - start;

  for (std::uint32_t i = 0; i < size; ++i) {
    auto b = i;
    b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
    b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
    b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
    b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
    b = ((b >> 16) | (b << 16)) >> (32 - std::uint32_t(log2(size)));
    if (b > i) {
      std::swap(start[b], start[i]);
    }
  }
}

template <typename Iter, typename Iter_end>
void iterative_cooley_tukey(Iter start, Iter_end end) {
  // Bit reverse the array.
  bit_reverse(start, end);

  //Preform the butterfly on the array.
  auto size = end - start;
  for (std::size_t stride = 2; stride <= size; stride *= 2) {
    auto w = exp(c64(0, -2.0 * pi<double>() / stride));
    for (std::size_t j = 0; j < size; j += stride) {
      auto v = c64(1.0);
      for (std::size_t k = 0; k < stride / 2; k++) {
        start[k + j + stride / 2] =
          start[k + j] - v * start[k + j + stride / 2];
        start[k + j] -= (start[k + j + stride / 2] - start[k + j]);
        v *= w;
      }
    }
  }
}

int main() {
  // Initalizing the FFT inputs.
  auto random_number_generator = std::mt19937_64();
  auto generate_random_double = [&]() {
    auto rn = random_number_generator();
    return double(rn) / double(UINT64_MAX);
  };

  std::array<c64, 64> initial;

  for (auto& el : initial) {
    el = generate_random_double();
  }

  auto recursive = initial;
  auto iterative = initial;

  // Preform an FFT on the arrays.
  cooley_tukey(recursive.begin(), recursive.end());
  iterative_cooley_tukey(iterative.begin(), iterative.end());

  // Check if the arrays are approximate.
  std::cout
    << std::right
    << std::setw(16) << "idx"
    << std::setw(16) << "rec"
    << std::setw(16) << "it"
    << std::setw(16) << "subtracted"
    << '\n';
  for (int i = 0; i < initial.size(); ++i) {
    auto rec = recursive[i];
    auto it = iterative[i];
    std::cout
      << std::setw(16) << i
      << std::setw(16) << std::abs(rec)
      << std::setw(16) << std::abs(it)
      << std::setw(16) << (std::abs(rec) - std::abs(it))
      << '\n';
  }
}
