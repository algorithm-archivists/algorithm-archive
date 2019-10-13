// note this example is inplace and destructive
thomas = (a, b, c, d) => {

    // set the initial elements
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    n = d.length // number of equations to solve
    for (i = 1; i < n; i++) {
        scale = 1 / (b[i] - c[i-1] * a[i]) // scale factor for c and d
        c[i] *= scale
        d[i] = (d[i] - a[i] * d[i-1]) * scale
    }

    // do the back substitution
    for (i = n-2; i >= 0; i--) {
        d[i] -= c[i] * d[i+1]
    }

    return d
}

/*
  example for matrix
  [1  4  0][x]   [7]
  [2  3  5][y] = [5]
  [0  3  6][z]   [3]

                  [.8666]
  soln will equal [1.533]
                  [-.266]
  note we index a from 1 and c from 0
*/

a = [0, 2, 3]
b = [1, 3, 6]
c = [4, 5, 0]
d = [7, 5, 3]

soln = thomas(a, b, c, d)
console.log(soln)