
function bubble_sort(arr)
	for i = 1,#arr do
		for j = 1,#arr-1 do
			if arr[j] > arr[j+1] then
				local temp = arr[j]
				arr[j] = arr[j+1]
				arr[j+1] = temp
			end
		end
	end
end
