#include <cmath>
#include <iostream>
#include <utility>

// Euclidean algorithm using modulus
int euclid_mod(int a, int b) {
  a = std::abs(a);
  b = std::abs(b);

  while (b != 0) {
    a = std::exchange(b, a % b);
  }

  return a;
}

// Euclidean algorithm with subtraction
int euclid_sub(int a, int b) {
  a = std::abs(a);
  b = std::abs(b);

  while (a != b) {
    if (a > b) {
      a -= b;
    } else {
      b -= a;
    }
  }

  return a;
}

int main() {
  auto check1 = euclid_mod(64 * 67, 64 * 81);
  auto check2 = euclid_sub(128 * 12, 128 * 77);

  std::cout << check1 << '\n';
  std::cout << check2 << '\n';
}
