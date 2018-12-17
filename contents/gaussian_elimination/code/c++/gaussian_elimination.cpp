#include <iostream>
#include <algorithm>
#include <stdexcept>
#include <vector>
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


  void gaussianElimination(vector<vector<long double>> &eqns, int varc)
  {
    // 'eqns' is the matrix, 'varc' is no. of vars
    for (int i = 0; i < varc - 1; i++)
    {
      if (eqns[i][i] == 0)
      {
        for (int j = i + 1; j < varc; j++)
        {
          if (eqns[j][i] != 0)
          {
            swapRow(eqns[j], eqns[i], i);
            break;
          }
        }

        if (eqns[i][i] == 0)
          throw runtime_error("Singular matrix");
        // Cannot solve since coefficient of one variable is always zero
      }

      for (int j = i + 1; j < varc; j++)
        if (eqns[j][i])
          subRow(eqns[j], eqns[i], (eqns[j][i] / eqns[i][i]), i);
    }

    if (eqns[varc - 1][varc - 1] == 0)
    {
      if (eqns[varc - 1][varc] == 0)
        throw runtime_error("Singular matrix");
      // Cannot solve since final equation is '0*xn = 0'
      else
        throw runtime_error("No solutions");
      // Cannot solve since final equation is '0*xn = c'
    }
  }


  void gaussJordan(vector<vector<long double>> &eqns, int varc)
  {
    // 'eqns' is the (Row-echelon) matrix, 'varc' is no. of vars
    for (int i = varc - 1; i >= 0; i--)
    {
      eqns[i][varc] /= eqns[i][i];
      eqns[i][i] = 1;
      for (int j = i - 1; j >= 0; j--)
      {
        eqns[j][varc] -= eqns[j][i] * eqns[i][varc];
        eqns[j][i] = 0;
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
    this->gaussianElimination(eqns, varc);
    this->gaussJordan(eqns, varc);

    vector<long double> ans(varc);
    for (int i = 0; i < varc; i++)
      ans[i] = eqns[i][varc];
    return ans;
  }

  vector<long double> solveByBacksubs(vector< vector<long double> > eqns, int varc)
  {
    this->gaussianElimination(eqns, varc);
    vector<long double> ans = this->backSubs(eqns, varc);
    return ans;
  }
};

int main() {
  int varc = 3;
  vector< vector<long double> > equations { { 2 , 3, 4, 6 },
                                            { 1 , 2, 3, 4 },
                                            { 3 ,-4, 0, 10 }};
  EquationSolver solver;
  vector<long double> ans;
  try
  {
    ans = solver.solveByGaussJordan(equations, varc);
  }
  catch(runtime_error e)
  {
    cout << "Error found: " << e.what() << endl;
    return -1;
  }

  cout << "The solution is," << endl
       << "x == " << ans[0] << endl
       << "y == " << ans[1] << endl
       << "z == " << ans[2] << endl;
}
