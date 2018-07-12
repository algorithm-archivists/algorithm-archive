// written by Gathros, modernized by Nicole Mazzuca.

#include <algorithm>
#include <array>
#include <complex>
#include <cstdint>
#include <numeric>
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

namespace ct {

// `recursive` does the cooley-tukey algorithm, recursively
template <typename Iter>
void recursive(Iter first, Iter last) {
  std::size_t size = last - first;
  auto half_size = size / 2;
  if (half_size >= 1) {
    // split the range, with even indices going in the first half,
    // and odd indices going in the last half.
    auto temp = std::vector<c64>(half_size);
    for (std::size_t i = 0; i < half_size; ++i) {
      temp[i] = first[i * 2 + 1];
      first[i] = first[i * 2];
    }
    for (std::size_t i = 0; i < half_size; ++i) {
      first[i + size / 2] = temp[i];
    }

    // recurse the splits and butterflies in each half of the range
    auto split = first + half_size;
    recursive(first, split);
    recursive(split, last);

    // now combine each of those halves with the butterflies
    for (std::size_t k = 0; k < half_size; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double>() * k / size));

      auto& bottom = first[k];
      auto& top = first[k + half_size];
      top = bottom - w * top;
      bottom -= top - bottom;
    }
  }
}

template <typename Iter>
void sort_by_bit_reverse(Iter first, Iter last) {
  // sorts the range [first, last) in bit-reversed order,
  // by the method suggested by the FFT

  std::size_t size = last - first;
  auto log = static_cast<std::size_t>(std::log2(size));
  auto count = log - 1;
  auto bitmask = (1 << log) - 1;

  for (std::size_t i = 0; i < size; ++i) {
    auto n = i;
    auto a = i;
    auto current_count = count;

    // this left shifts i and right shifts the shifted bit to n by bitwise or
    n >>= 1;
    while (n > 0) {
      a = (a << 1) | (n & 1);
      --current_count;
      n >>= 1;
    }
    n = (a << current_count) & bitmask;

    if (n > i) {
      swap(first[i], first[n]);
    }
  }
}

// `iterative` does the cooley-tukey algorithm iteratively
template <typename Iter>
void iterative(Iter first, Iter last) {
  sort_by_bit_reverse(first, last);

  // perform the butterfly on the range
  std::size_t size = last - first;
  for (std::size_t stride = 2; stride <= size; stride *= 2) {
    auto w = exp(c64(0, -2.0 * pi<double>() / stride));
    for (std::size_t j = 0; j < size; j += stride) {
      auto v = c64(1.0);
      for (std::size_t k = 0; k < stride / 2; k++) {
        first[k + j + stride / 2] =
            first[k + j] - v * first[k + j + stride / 2];
        first[k + j] -= (first[k + j + stride / 2] - first[k + j]);
        v *= w;
      }
    }
  }
}

template <typename It, typename F>
auto sum(It const first, It const last, F f) {
  using return_type = decltype(f(0, *first));
  auto ret = return_type(0);

  std::size_t i = 0;
  for (auto it = first; it < last; ++it, ++i) {
    ret += f(i, *it);
  }

  return ret;
}

template <typename Iter>
void discrete(Iter const first, Iter const last) {
  using namespace std::literals;

  auto const original = std::vector<c64>(first, last);
  auto const size = original.size();

  auto const i2pi = -2.0i * pi<double>();

  for (std::size_t i = 0; i < size; ++i) {
  for (std::size_t i = 0; i < size; ++i) {
    first[i] =
        sum(begin(original), end(original), [&](auto const k, auto const el) {
          return el * std::exp(i2pi * double(i * k) / double(size));
        });
  }

  }
}

} // namespace ct

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
  auto discrete = initial;

  // Preform an FFT on the arrays.
  ct::recursive(begin(recursive), end(recursive));
  ct::iterative(begin(iterative), end(iterative));
  ct::discrete(begin(discrete), end(discrete));

  // Check if the arrays are approximately equivalent
  std::cout << std::right << std::setw(12) << "idx";
  std::cout << std::setw(12) << "|recursive|";
  std::cout << std::setw(12) << "|iterative|";
  std::cout << std::setw(12) << "|discrete|";
  std::cout << std::setw(12) << "error" << '\n';
  for (std::size_t i = 0; i < initial.size(); ++i) {
    auto rec = recursive[i];
    auto it = iterative[i];
    auto disc = discrete[i];
    std::cout << std::setw(12) << i;
    std::cout << std::setw(12) << std::abs(rec);
    std::cout << std::setw(12) << std::abs(it);
    std::cout << std::setw(12) << std::abs(disc);

    auto err = std::max(
        {std::abs(rec - it), std::abs(it - disc), std::abs(disc - rec)});
    std::cout << std::setw(12) << err << '\n';
  }
}
