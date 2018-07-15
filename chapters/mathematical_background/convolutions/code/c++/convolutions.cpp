#include <algorithm>
#include <array>
#include <cassert>
#include <complex>
#include <cstdint>
#include <iterator>
#include <vector>

// These headers are for presentation not for the algorithm.
#include <iomanip>
#include <iostream>
#include <random>

using std::begin;
using std::end;
using std::swap;

using std::ptrdiff_t;
using std::size_t;

using c64 = std::complex<double>;
template <typename T>
constexpr T pi() {
  return 3.14159265358979323846264338327950288419716;
}

// This section is not a part of the algorithm
template <typename Iter>
void fft(Iter const first, Iter const last) {
  auto const size = last - first;
  if (size >= 2) {
    auto temp = std::vector<c64>(size / 2);
    for (ptrdiff_t i = 0; i < size / 2; ++i) {
      temp[i] = first[i * 2 + 1];
      first[i] = first[i * 2];
    }
    for (ptrdiff_t i = 0; i < size / 2; ++i) {
      first[i + size / 2] = temp[i];
    }

    auto const split = first + size / 2;
    fft(first, split);
    fft(split, last);

    for (ptrdiff_t k = 0; k < size / 2; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double>() * k / size));

      auto& bottom = first[k];
      auto& top = first[k + size / 2];
      top = bottom - w * top;
      bottom -= top - bottom;
    }
  }
}

template <typename Iter>
void inverse_fft(Iter const first, Iter const last) {
  std::for_each(first, last, [](auto& it) { it = std::conj(it); });

  fft(first, last);

  auto const size = static_cast<c64>(last - first);
  std::for_each(first, last, [&](auto& it) { it = std::conj(it) / size; });
}

// This section is a part of the algorithm

template <typename S1, typename S2, typename Out>
void conv(
    S1 const s1,
    S1 const s1_last,
    S2 const s2,
    S2 const s2_last,
    Out const out) {
  auto const size1 = s1_last - s1;
  auto const size2 = s2_last - s2;
  auto const size = size1 + size2;

  for (ptrdiff_t i = 0; i < size; ++i) {
    c64 sum = 0;
    for (ptrdiff_t j = 0; j < i; ++j) {
      if (j < size1) {
        sum += s1[j] * s2[i - j];
      }
    }
    out[i] = sum;
  }
}

template <typename S1, typename S2, typename Out>
void conv_fft(S1 const s1, S1 const s1_last, S2 const s2, Out const out) {
  auto const size = s1_last - s1;

  fft(s1, s1_last);
  fft(s2, s2 + size);

  auto s1_it = s1;
  auto s2_it = s2;
  auto out_it = out;
  for (; s1_it != s1_last; ++s1_it, ++s2_it, ++out_it) {
    *out_it = *s1_it * *s2_it;
  }

  inverse_fft(out, out_it);
}

int main() {
  auto signal1 = std::array<c64, 64>();
  std::fill(begin(signal1) + 16, begin(signal1) + 48, 1);

  auto signal2 = signal1;

  auto out1 = std::array<c64, 128>();
  conv(begin(signal1), end(signal1), begin(signal2), end(signal2), begin(out1));

  auto signal3 = std::array<c64, 128>();
  std::copy(begin(signal1), end(signal1), begin(signal3));
  auto signal4 = signal3;

  auto out2 = std::array<c64, 128>();
  conv_fft(begin(signal3), end(signal3), begin(signal4), begin(out2));

  std::cout << std::right << std::setw(16) << "i" << std::setw(16)
            << "subtracted" << '\n';

  for (size_t i = 0; i < signal1.size(); ++i) {
    std::cout << std::setw(16) << i << std::setw(16)
              << (std::abs(out1[i]) - std::abs(out2[i])) << '\n';
  }
}
