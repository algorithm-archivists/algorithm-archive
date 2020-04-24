#!/usr/bin/env ruby

def bubble_sort(arr)
	(0..arr.length - 1).each do
		(0..arr.length - 2).each do |k|
			if arr[k] > arr[k + 1]
				arr[k + 1], arr[k] = arr[k], arr[k + 1]
			end
		end
	end

	return arr
end

def main
	range = [200, 79, 69, 45, 32, 5, 15, 88, 620, 125]	
	puts "The range before sorting is #{range}"
	range = bubble_sort(range)
	puts "The range after sorting is #{range}"
end

main()
