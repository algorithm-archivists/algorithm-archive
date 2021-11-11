#include <iostream>
#include <vector>

void thomas(
    std::vector<double> const& a,
    std::vector<double> const& b,
    std::vector<double> const& c,
    std::vector<double>& x) {
  const auto size = a.size();
  auto y = std::vector<double>(size, 0.0);

  y[0] = c[0] / b[0];
  x[0] = x[0] / b[0];

  for (std::size_t i = 1; i < size; ++i) {
    double scale = 1.0 / (b[i] - a[i] * y[i - 1]);
    y[i] = c[i] * scale;
    x[i] = (x[i] - a[i] * x[i - 1]) * scale;
  }

  for (int i = static_cast<int>(size) - 2; i >= 0; --i) {
    auto iu = static_cast<std::size_t>(i);
    x[iu] -= y[iu] * x[iu + 1];
  }
}

int main() {
  const std::vector<double> a = {0.0, 2.0, 3.0};
  const std::vector<double> b = {1.0, 3.0, 6.0};
  const std::vector<double> c = {4.0, 5.0, 0.0};
  std::vector<double> x = {7.0, 5.0, 3.0};

  std::cout << "The system" << std::endl;
  std::cout << "[1.0  4.0  0.0][x] = [7.0]" << std::endl;
  std::cout << "[2.0  3.0  5.0][y] = [5.0]" << std::endl;
  std::cout << "[0.0  3.0  6.0][z] = [3.0]" << std::endl;
  std::cout << "has the solution" << std::endl;

  thomas(a, b, c, x);

  for (auto const& val : x) {
    std::cout << "[" << val << "]" << std::endl;
  }

  return 0;
}
