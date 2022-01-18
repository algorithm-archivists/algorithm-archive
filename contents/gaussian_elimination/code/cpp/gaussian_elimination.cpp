#include <algorithm>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <vector>


void gaussianElimination(std::vector<std::vector<double> > &eqns) {
  // 'eqns' is the matrix, 'rows' is no. of vars
  std::size_t rows = eqns.size(), cols = eqns[0].size();

  for (std::size_t i = 0; i < rows - 1; i++) {
      std::size_t pivot = i;

    for (std::size_t j = i + 1; j < rows; j++) {
      if (fabs(eqns[j][i]) > fabs(eqns[pivot][i])) pivot = j;
    }

    if (eqns[pivot][i] == 0.0)
      continue;  // But continuing to simplify the matrix as much as possible

    if (i != pivot)  // Swapping the rows if new row with higher maxVals is found
      std::swap(eqns[pivot], eqns[i]);  // C++ swap function

    for (std::size_t j = i + 1; j < rows; j++) {
      double scale = eqns[j][i] / eqns[i][i];

      for (std::size_t k = i + 1; k < cols; k++)   // k doesn't start at 0, since
        eqns[j][k] -= scale * eqns[i][k];  // values before from 0 to i
                                           // are already 0
      eqns[j][i] = 0.0;
    }
  }
}

void gaussJordan(std::vector<std::vector<double> > &eqns) {
  // 'eqns' is the (Row-echelon) matrix, 'rows' is no. of vars
  std::size_t rows = eqns.size();

  for (std::size_t i = rows - 1; i < rows; i--) {

    if (eqns[i][i] != 0) {

      eqns[i][rows] /= eqns[i][i];
      eqns[i][i] = 1;  // We know that the only entry in this row is 1

      // subtracting rows from below
      for (std::size_t j = i - 1; j < i; j--) {
        eqns[j][rows] -= eqns[j][i] * eqns[i][rows];
        eqns[j][i] = 0;  // We also set all the other values in row to 0 directly
      }
    }
  }
}

std::vector<double> backSubs(const std::vector<std::vector<double> > &eqns) {
  // 'eqns' is matrix, 'rows' is no. of variables
  std::size_t rows = eqns.size();

  std::vector<double> ans(rows);
  for (std::size_t i = rows - 1; i < rows; i--) {
    double sum = 0.0;

    for (std::size_t j = i + 1; j < rows; j++) sum += eqns[i][j] * ans[j];

    if (eqns[i][i] != 0)
      ans[i] = (eqns[i][rows] - sum) / eqns[i][i];
    else
      return std::vector<double>(0);
  }
  return ans;
}


void printMatrix(const std::vector<std::vector<double> > &matrix) {
  for (std::size_t row = 0; row < matrix.size(); row++) {
    std::cout << "[";

    for (std::size_t col = 0; col < matrix[row].size() - 1; col++)
      std::cout << std::setw(8) << std::fixed << std::setprecision(3)
                << matrix[row][col];

    std::cout << " |" << std::setw(8) << std::fixed << std::setprecision(3)
              << matrix[row].back() << " ]" << std::endl;
  }
}


int main() {
  std::vector<std::vector<double> > equations{
      {2, 3, 4, 6},
      {1, 2, 3, 4},
      {3, -4, 0, 10}};

  std::cout << "Initial matrix:" << std::endl;
  printMatrix(equations);
  std::cout << std::endl;

  gaussianElimination(equations);
  std::cout << "Matrix after gaussian elimination:" << std::endl;
  printMatrix(equations);
  std::cout << std::endl;

  std::vector<double> ans = backSubs(equations);
  std::cout << "Solution from backsubstitution" << std::endl;
  std::cout << "x = " << ans[0] << ", y = " << ans[1] << ", z = " << ans[2]
            << std::endl
            << std::endl;

  gaussJordan(equations);
  std::cout << "Matrix after Gauss Jordan:" << std::endl;
  printMatrix(equations);
  std::cout << std::endl;
}
