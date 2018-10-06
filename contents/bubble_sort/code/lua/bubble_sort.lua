function bubble_sort(arr)
  for i = 1,#arr-1 do
    for j = 1,#arr-1 do
      if arr[j] > arr[j+1] then
        arr[j], arr[j+1] = arr[j+1], arr[j]
      end
    end
  end
end

local arr = {1, 45, 756, 4569, 56, 3, 8, 5, -10, -4}
print(("Unsorted array: {%s}"):format(table.concat(arr,", ")))

bubble_sort(arr)

print(("Sorted array: {%s}"):format(table.concat(arr,", ")))
