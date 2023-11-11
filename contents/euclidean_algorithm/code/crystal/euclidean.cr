def euclid_mod(a : Int, b : Int)
  a = a.abs
  b = b.abs

  loop do
    b, a = a % b, b
    break if b == 0
  end

  return a
end

def euclid_sub(a : Int, b : Int)
  a = a.abs
  b = b.abs

  loop do
    if a > b
      a -= b
    else
      b -= a
    end
    break if a == b
  end

  return a
end

def main
  euc_mod = euclid_mod(64 * 67, 64 * 81)
  euc_sub = euclid_sub(128 * 12, 128 * 77)

  puts "Modulus-based euclidean algorithm result: #{euc_mod}"
  puts "Subtraction-based euclidean algorithm result: #{euc_sub}"
end

main