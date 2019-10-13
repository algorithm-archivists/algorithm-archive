# note this example is inplace and destructive
def thomas(a, b, c, d)
  # set the initial elements
  c[0] = c[0] / b[0]
  d[0] = d[0] / b[0]

  n = d.length # number of equations to solve
  (1...n).each do |i|
    scale = 1 / (b[i] - c[i - 1] * a[i]) # scale factor for c and d
    c[i] *= scale
    d[i] = (d[i] - a[i] * d[i - 1]) * scale
  end

  # do the back substitution
  (n - 2).downto(0).each do |j|
    d[j] -= c[j] * d[j + 1]
  end

  d
end

# example for matrix
# [1  4  0][x]   [7]
# [2  3  5][y] = [5]
# [0  3  6][z]   [3]

#                 [.8666]
# soln will equal [1.533]
#                 [-.266]
# note we index a from 1 and c from 0

a = [0.0, 2.0, 3.0]
b = [1.0, 3.0, 6.0]
c = [4.0, 5.0, 0.0]
d = [7.0, 5.0, 3.0]

soln = thomas(a, b, c, d)
puts soln
