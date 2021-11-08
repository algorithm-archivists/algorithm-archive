#include <iostream>
#include <vector>
#include <cstring>

void thomas(std::vector<double> const a, std::vector<double> const b, std::vector<double> const c, std::vector<double>& x) {
    int size = a.size();
    double y[size];
    memset(y, 0, size * sizeof(double));

    y[0] = c[0] / b[0];
    x[0] = x[0] / b[0];

    for (size_t i = 1; i < size; ++i) {
        double scale = 1.0 / (b[i] - a[i] * y[i - 1]);
        y[i] = c[i] * scale;
        x[i] = (x[i] - a[i] * x[i - 1]) * scale;
    }

    for (int i = size - 2; i >= 0; --i) {
        x[i] -= y[i] * x[i + 1];
    }
}

int main() {
    std::vector<double> a = {0.0, 2.0, 3.0};
    std::vector<double> b = {1.0, 3.0, 6.0};
    std::vector<double> c = {4.0, 5.0, 0.0};
    std::vector<double> x = {7.0, 5.0, 3.0};

    std::cout << "The system" << std::endl; 
    std::cout << "[1.0  4.0  0.0][x] = [7.0]" << std::endl;
    std::cout << "[2.0  3.0  5.0][y] = [5.0]" << std::endl;
    std::cout << "[0.0  3.0  6.0][z] = [3.0]" << std::endl;
    std::cout << "has the solution" << std::endl;

    thomas(a, b, c, x);

    for (size_t i = 0; i < 3; ++i) {
        std::cout << "[" << x[i] << "]" << std::endl;
    }

    return 0;
}
