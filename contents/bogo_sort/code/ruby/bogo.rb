#!/usr/bin/env ruby

def is_sorted(a)
  a.each_cons.all? { |(l, r)| l <= r }
end

def bogo_sort(a)
  a.shuffle! until is_sorted a
end

def main
	a = [1, 1, 0, 3, 7]
	
	puts "Unsorted"
	print a
	
	bogo_sort a
	
	puts "\n\nSorted" 
	print a 
end

main

