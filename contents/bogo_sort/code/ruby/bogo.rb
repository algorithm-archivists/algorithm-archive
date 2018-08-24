#!/usr/bin/env ruby

def is_sorted(a)
  for i in 0...a.length-1
    if a[i+1] < a[i]
	  return false
	end
  end
  return true
end

def bogo_sort(a)
  while !is_sorted(a)
    a.shuffle!
  end
end

def main()
	a = [1, 1, 0, 3, 7]
	bogo_sort(a)
	print a
end

main()