#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>
#include <iomanip>

void gaussian_elimination(std::vector<double>& a, int cols) {
  int rows = a.size() / cols;

  int row = 0;

  // Main loop going through all columns
  for (int col = 0; col < cols - 1; ++col) {
    // Step 1: finding the maximum element for each column
    int max_index = row;
    for (int r = row + 1; r < rows; ++r) {
      if (a[col + max_index * cols] < std::abs(a[col + r * cols])) {
        max_index = r;
      }
    }

    // Check to make sure matrix is good!
    if (a[col + max_index * cols] == 0) {
      std::cout << "matrix is singular!\n";
      continue;
    }

    // Step 2: swap row with highest value for that column to the top
    if (row != max_index) {
      for (int c = 0; c < cols; ++c)
        std::swap(a[c + row * cols], a[c + max_index * cols]);
    }

    // Loop for all remaining rows
    for (int i = row + 1; i < rows; ++i) {

      // Step 3: finding fraction
      auto fraction = a[col + i * cols] / a[col + row * cols];

      // loop through all columns for that row
      for (int j = col + 1; j < cols; ++j) {

        // Step 4: re-evaluate each element
        a[j + i * cols] -= a[j + row * cols] * fraction;
      }

      // Step 5: Set lower elements to 0
      a[col + i * cols] = 0;
    }
    ++row;
  }
}

std::vector<double> back_substitution(const std::vector<double>& a, int cols) {
  int rows = a.size() / cols;

  // Creating the solution Vector
  std::vector<double> soln(rows);

  for (int i = rows - 1; i >= 0; --i) {
    auto sum = 0.0;
    for (int j = cols - 2; j > i; --j) {
      sum += soln[j] * a[j + i * cols];
    }

    soln[i] = (a[cols - 1 + i * cols] - sum) / a[i + i * cols];
  }

  return soln;
}

void gauss_jordan_elimination(std::vector<double>& a, int cols) {
  // After this, we know what row to start on (r-1)
  // to go back through the matrix
  int row = 0;
  for (int col = 0; col < cols - 1; ++col) {
    if (a[col + row * cols] != 0) {

      // divide row by pivot and leaving pivot as 1
      for (int i = cols - 1; i >= col; --i)
        a[i + row * cols] /= a[col + row * cols];

      // subtract value from above row and set values above pivot to 0
      for (int i = 0; i < row; ++i)
        for (int j = cols - 1; j >= col; --j)
          a[j + i * cols] -= a[col + i * cols] * a[j + row * cols];
      ++row;
    }
  }
}

void print_matrix(const std::vector<double>& a, int cols) {
  int rows = a.size() / cols;
  for (int i = 0; i < rows; ++i) {
    std::cout << "[";
    for (int j = 0; j < cols; ++j) {
      std::cout << std::fixed << a[j + i * cols] << "\t";
    }
    std::cout << "]\n";
  }
}

int main() {
  std::vector<double> a = { 2, 3, 4, 6,
                            1, 2, 3, 4,
                            3, -4, 0, 10 };
  const int cols = 4;
  if (a.size() % cols != 0)
  {
    std::cout << "The input dimentions are incorrect\n";
    return 1;
  }

  gaussian_elimination(a, cols);
  print_matrix(a, cols);

  auto soln = back_substitution(a, 4);

  for (auto element : soln)
    std::cout << element << std::endl;

  gauss_jordan_elimination(a, cols);
  print_matrix(a, cols);

  soln = back_substitution(a, 4);

  for (auto element : soln)
    std::cout << element << std::endl;
}
