#!/usr/bin/env ruby

def is_sorted(a)
  a.each_cons(2).all? { |(l, r)| l <= r }
end

def bogo_sort(a)
  a.shuffle! until is_sorted a
end

a = [1, 1, 0, 3, 7]

puts "Unsorted"
p a

bogo_sort a

puts "Sorted" 
p a 
