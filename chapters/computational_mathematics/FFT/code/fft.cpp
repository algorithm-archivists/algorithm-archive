#include <iostream>
#include <iomanip>
#include <complex>
#include <vector>
#include <random>
#include <array>
#include <cstdint>

/* TODO: add some comments here before actually merging it */
/* TODO: in general, more commenting is good */
struct range {
  std::size_t begin;
  std::size_t end;
};

template <typename T>
class span {
  T* data_;
  std::size_t size_;
public:
  template <std::size_t N>
  span(std::array<T, N>& arr) : data_(arr.data()), size_(N) {}

  explicit span(T* ptr, std::size_t len) : data_(ptr), size_(len) {}

  T* data() { return data_; }

  std::size_t size() const { return size_; }

  T& operator[](std::size_t i) { return data_[i]; }

  span<T> operator[](range r) {
    return span<T>(data() + r.begin, r.end - r.begin);
  }
};

using c64 = std::complex<double>;
template <typename T>
constexpr T pi = 3.14159265358979323846264338327950288419716;

void cooley_tukey(span<c64> original, std::vector<c64>&& temp = {}) {
  auto size = original.size();
  if (size >= 2) {
    temp.reserve(size / 2);
    for (std::size_t i = 0; i < size / 2; ++i) {
      temp[i] = original[i * 2 + 1];
      original[i] = original[i * 2];
    }
    for (std::size_t i = 0; i < size / 2; ++i) {
      original[i + size / 2] = temp[i];
    }

    cooley_tukey(original[range{0, size / 2}], std::move(temp));
    cooley_tukey(original[range{size / 2, size}], std::move(temp));

    for (std::size_t k = 0; k < size / 2; ++k) {
      auto w = std::exp(c64(0, -2.0 * pi<double> * k / size));
      original[k + size / 2] = original[k] - w * original[k + size / 2];
      original[k] -= (original[k + size / 2] - original[k]);
    }
  }
}

void bit_reverse(span<c64> original) {
  auto size = original.size();

  for (std::uint32_t i = 0; i < size; ++i) {
    auto b = i;
    // Reverse bits
    b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
    b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
    b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
    b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
    b = ((b >> 16) | (b << 16)) >> (32 - std::uint32_t(log2(size)));
    if (b > i) {
      std::swap(original[b], original[i]);
    }
  }
}

void iterative_cooley_tukey(span<c64> original) {
  bit_reverse(original);

  auto size = original.size();
  for (std::size_t stride = 2; stride <= size; stride *= 2) {
    auto w = exp(c64(0, -2.0 * pi<double> / stride));
    for (std::size_t j = 0; j < size; j += stride) {
      auto v = c64(1.0);
      for (std::size_t k = 0; k < stride / 2; k++) {
        original[k + j + stride / 2] =
          original[k + j] - v * original[k + j + stride / 2];
        original[k + j] -= (original[k + j + stride / 2] - original[k + j]);
        v *= w;
      }
    }
  }
}

int main() {
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

  cooley_tukey(recursive);
  iterative_cooley_tukey(iterative);

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
