def thomas(a, b, c, d)
  c_prime = c.dup
  x = d.dup

  # Setting initial elements
  c_prime[0] /= b[0]
  x[0] /= b[0]

  1.upto(a.size - 1) do |i|
    # Scale factor is for c_prime and x
    scale = 1.0 / (b[i] - c_prime[i - 1]*a[i])
    c_prime[i] *= scale
    x[i] = (x[i] - a[i] * x[i - 1]) * scale
  end

  # Back-substitution
  (a.size - 2).downto(0) do |i|
    x[i] -= (c_prime[i] * x[i + 1])
  end

  x
end

def main
  a = [0.0, 2.0, 3.0]
  b = [1.0, 3.0, 6.0]
  c = [4.0, 5.0, 0.0]
  d = [7.0, 5.0, 3.0]

  puts "The system"
  puts [b[0], c[0], "",   "|", d[0]].join("\t")
  puts [a[1], b[1], c[1], "|", d[1]].join("\t")
  puts ["",   a[2], b[2], "|", d[2]].join("\t")
  puts "Has the solution:"

  soln = thomas(a, b, c, d)

  puts soln.join("\t")
end

main
