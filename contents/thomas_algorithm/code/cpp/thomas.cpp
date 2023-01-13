#include <cstddef>
#include <iostream>
#include <vector>

void thomas(
    std::vector<double> const& a,
    std::vector<double> const& b,
    std::vector<double> const& c,
    std::vector<double>& x) {
  auto y = std::vector<double>(a.size(), 0.0);

  y[0] = c[0] / b[0];
  x[0] = x[0] / b[0];

  for (std::size_t i = 1; i < a.size(); ++i) {
    const auto scale = 1.0 / (b[i] - a[i] * y[i - 1]);
    y[i] = c[i] * scale;
    x[i] = (x[i] - a[i] * x[i - 1]) * scale;
  }

  for (std::size_t i = a.size() - 2; i < a.size(); --i) {
    x[i] -= y[i] * x[i + 1];
  }
}

int main() {
  const std::vector<double> a = {0.0, 2.0, 3.0};
  const std::vector<double> b = {1.0, 3.0, 6.0};
  const std::vector<double> c = {4.0, 5.0, 0.0};
  std::vector<double> x = {7.0, 5.0, 3.0};

  std::cout << "The system\n";
  std::cout << "[1.0  4.0  0.0][x] = [7.0]\n";
  std::cout << "[2.0  3.0  5.0][y] = [5.0]\n";
  std::cout << "[0.0  3.0  6.0][z] = [3.0]\n";
  std::cout << "has the solution:\n";

  thomas(a, b, c, x);

  for (auto const& val : x) {
    std::cout << "[" << val << "]\n";
  }

  return 0;
}
