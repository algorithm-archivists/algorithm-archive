#include <iostream>
#include <algorithm>
#include <stdexcept>
#include <vector>
#include <cmath>
using namespace std;

class EquationSolver
{
private:

  // subtract row from another row after multiplcation
  void subRow(vector<long double> &toBeSubtracted, const vector<long double> &toSubtract,
              long double mult, int start)
  {
    for (int i = start; i < toBeSubtracted.size(); i++)
      toBeSubtracted[i] -= mult * toSubtract[i];
  }
  // A start variable is given since if we know all values are 0, we don't need
  // to subtract.


  // swap two rows
  void swapRow(vector<long double> &eqn1, vector<long double> &eqn2, int start)
  {
    for (int i = start; i < eqn1.size(); i++)
      swap(eqn1[i], eqn2[i]);
  }
  // A start variable is given since if we know all values are 0, we don't need
  // to swap.


  int gaussianElimination(vector<vector<long double>> &eqns, int varc)
  {
    // 'eqns' is the matrix, 'varc' is no. of vars
    int err = 0; // marker for if matrix is singular
    for (int i = 0; i < varc - 1; i++)
    {
      long double pivot = fabsl(eqns[i][i]);
      int newRow = i;
      for (int j = i + 1; j < varc; j++)
      {
        if (fabsl(eqns[j][i]) > pivot)
        {
          pivot = fabsl(eqns[j][i]);
          newRow = j;
        }
      }

      if (pivot == 0.0)
      {
        err = 1; // Marking that matrix is singular
        continue;
      }
      if (newRow != i)
        swapRow(eqns[newRow], eqns[i], i);

      for (int j = i + 1; j < varc; j++)
        if (eqns[j][i])
          subRow(eqns[j], eqns[i], (eqns[j][i] / eqns[i][i]), i);
    }

    if (eqns[varc - 1][varc - 1] == 0 || err)
    {
      if (eqns[varc - 1][varc] == 0 || err)
        return 1; // Error code: Singular matrix
      else
        return 2; // Error code: No solutions
      // Cannot solve since final equation is '0*xn = c'
    }
    return 0; // Successful
  }


  void gaussJordan(vector<vector<long double>> &eqns, int varc)
  {
    // 'eqns' is the (Row-echelon) matrix, 'varc' is no. of vars
    for (int i = varc - 1; i >= 0; i--)
    {
      eqns[i][varc] /= eqns[i][i];
      eqns[i][i] = 1; // We know that the only entry in this row is 1

      for (int j = i - 1; j >= 0; j--) // subtracting rows from below
      {
        eqns[j][varc] -= eqns[j][i] * eqns[i][varc];
        eqns[j][i] = 0; // We also set all the other values in row to 0 directly
      }
    }
  }


  vector<long double> backSubs(vector<vector<long double>> &eqns, int varc)
  {
    // 'eqns' is matrix, 'varc' is no. of variables
    vector<long double> ans(varc);
    for (int i = varc - 1; i >= 0; i--)
    {
      long double sum = 0;
      for (int j = i + 1; j < varc; j++)
        sum += eqns[i][j] * ans[j];

      ans[i] = (eqns[i][varc] - sum) / eqns[i][i];
    }
    return ans;
  }

public:
  vector<long double> solveByGaussJordan(vector< vector<long double> > eqns, int varc)
  {
    int status = this->gaussianElimination(eqns, varc);
    switch (status)
    {
      case 0:
        break;

      case 1:
        throw runtime_error("Singular matrix");

      case 2:
        throw runtime_error("No solutions");
    }

    this->gaussJordan(eqns, varc);

    vector<long double> ans(varc);
    for (int i = 0; i < varc; i++)
      ans[i] = eqns[i][varc];
    return ans;
  }

  vector<long double> solveByBacksubs(vector< vector<long double> > eqns, int varc)
  {
    int status = this->gaussianElimination(eqns, varc);
    switch (status)
    {
      case 0:
        break;

      case 1:
        throw runtime_error("Singular matrix");

      case 2:
        throw runtime_error("No solutions");
    }

    vector<long double> ans = this->backSubs(eqns, varc);
    return ans;
  }
};

int main() {
  int varc = 3;
  vector< vector<long double> > equations { { 2, 3, 4, 6 },
                                            { 1, 2, 3, 4 },
                                            { 3, -4, 0, 10 }};
  EquationSolver solver;
  vector<long double> ans;
  try
  {
    ans = solver.solveByGaussJordan(equations, varc);
  }
  catch(runtime_error &e)
  {
    cout << "Error found: " << e.what() << endl;
    return -1;
  }

  cout << "The solution is (by Gauss-Jordan)," << endl
       << "x == " << ans[0] << endl
       << "y == " << ans[1] << endl
       << "z == " << ans[2] << endl << endl;

  try
  {
    ans = solver.solveByBacksubs(equations, varc);
  }
  catch (runtime_error &e)
  {
    cout << "Error found: " << e.what() << endl;
    return -1;
  }

  cout << "The solution is (by Backsubstitution)," << endl
       << "x == " << ans[0] << endl
       << "y == " << ans[1] << endl
       << "z == " << ans[2] << endl;
}
