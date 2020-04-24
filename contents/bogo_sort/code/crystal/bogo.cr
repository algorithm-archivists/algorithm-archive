def is_sorted?(a)
  0.upto(a.size - 2) do |i|
    if a[i] > a[i + 1]
      return false
    end
  end
  true
end

def bogo_sort!(a)
  while !is_sorted?(a)
    a.shuffle!
  end
end

def main
  a = [1.0, 3.0, 2.0, 4.0]
  bogo_sort!(a)
  puts a
end

main
