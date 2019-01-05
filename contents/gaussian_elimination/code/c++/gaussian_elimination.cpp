#include <algorithm>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <vector>


int gaussianElimination(std::vector< std::vector<double> > &eqns, int varc) {
  // 'eqns' is the matrix, 'varc' is no. of vars
  int err = 0; // marker for if matrix is singular
  for (int i = 0; i < varc - 1; i++) {
    double pivot = fabs(eqns[i][i]);
    int newRow = i;

    for (int j = i + 1; j < varc; j++) {
      if (fabs(eqns[j][i]) > pivot) {
        pivot = fabs(eqns[j][i]);
        newRow = j;
      }
    }

    if (pivot == 0.0) {
      err = 1; // Marking that matrix is singular
      continue;
    }

    if (newRow != i) // Swapping the rows if new row with higher pivot is found
      std::swap(eqns[newRow], eqns[i]); // C++ swap function

    for (int j = i + 1; j < varc; j++) {
      if (eqns[j][i]) {
        double mult = eqns[j][i] / eqns[i][i];
        eqns[j][i] = 0.0;
        for (int k = i + 1; k < (varc + 1); k++) // k doesn't start at 0, since
          eqns[j][k] -= mult * eqns[i][k];       // values before from 0 to i
      }                                          // are already 0
    }
  }

  if (eqns[varc - 1][varc - 1] == 0 || err) {
    if (eqns[varc - 1][varc] == 0 || err)
      return 1; // Error code: Singular matrix
    else
      return 2; // Error code: No solutions
    // Cannot solve since final equation is '0*xn = c'
  }

  return 0; // Successful
}


void gaussJordan(std::vector< std::vector<double> > &eqns, int varc) {
  // 'eqns' is the (Row-echelon) matrix, 'varc' is no. of vars
  for (int i = varc - 1; i >= 0; i--) {
    eqns[i][varc] /= eqns[i][i];
    eqns[i][i] = 1; // We know that the only entry in this row is 1

    // subtracting rows from below
    for (int j = i - 1; j >= 0; j--) {
      eqns[j][varc] -= eqns[j][i] * eqns[i][varc];
      eqns[j][i] = 0; // We also set all the other values in row to 0 directly
    }
  }
}


std::vector<double> backSubs(std::vector<std::vector<double>> &eqns, int varc)
{
  // 'eqns' is matrix, 'varc' is no. of variables
  std::vector<double> ans(varc);
  for (int i = varc - 1; i >= 0; i--) {
    double sum = 0;
    for (int j = i + 1; j < varc; j++)
      sum += eqns[i][j] * ans[j];

    ans[i] = (eqns[i][varc] - sum) / eqns[i][i];
  }
  return ans;
}


std::vector<double> solveByGaussJordan(std::vector<std::vector<double>> eqns, int varc) {
  int status = gaussianElimination(eqns, varc);
  switch (status) {
  case 0:
    break;

  case 1:
    std::cout << "Singular matrix" << std::endl;
    return std::vector<double>(0); // return empty vector in case of no solutions

  case 2:
    std::cout << "No solutions" << std::endl;
    return std::vector<double>(0); // return empty vectors in case of no solutions
  }

  gaussJordan(eqns, varc);

  std::vector<double> ans(varc);
  for (int i = 0; i < varc; i++)
    ans[i] = eqns[i][varc];
  return ans;
}


std::vector<double> solveByBacksubs(std::vector<std::vector<double>> eqns, int varc) {
  int status = gaussianElimination(eqns, varc);
  switch (status) {
  case 0:
    break;

  case 1:
    std::cout << "Singular matrix" << std::endl;
    return std::vector<double>(0); // return empty vectors in case of no solutions

  case 2:
    std::cout << "No solutions" << std::endl;
    return std::vector<double>(0); // return empty vectors in case of no solutions
  }

  std::vector<double> ans = backSubs(eqns, varc);
  return ans;
}


int main() {
  int varc = 3;
  std::vector< std::vector<double> > equations{
      {2, 3, 4, 6},
      {1, 2, 3, 4},
      {3, -4, 0, 10}};

  std::vector<double> ans;

  ans = solveByGaussJordan(equations, varc);

  if (ans.empty()) { // If ans is empty, then no unique solutions exist
    std::cout << "No solution" << std::endl;
  } else {
    std::cout << "The solution is (by Gauss Jordan)," << std::endl
         << "x == " << ans[0] << std::endl
         << "y == " << ans[1] << std::endl
         << "z == " << ans[2] << std::endl;
  }

  ans = solveByBacksubs(equations, varc);

  if (ans.empty()) { // If ans is empty, then no unique solutions exist
    std::cout << "No solution" << std::endl;
  } else {
    std::cout << "The solution is (by Backsubstitution)," << std::endl
         << "x == " << ans[0] << std::endl
         << "y == " << ans[1] << std::endl
         << "z == " << ans[2] << std::endl;
  }
}
