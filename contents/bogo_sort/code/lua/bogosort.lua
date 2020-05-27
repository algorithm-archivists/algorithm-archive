local function shuffle(arr)
  for i = 1, #arr-1 do
    local rand = math.random(i,#arr)
    arr[i], arr[rand] = arr[rand], arr[i]
  end
end

local function is_sorted(arr)
  for i = 1,#arr-1 do
    if arr[i] > arr[i+1] then
      return false
    end
  end
  return true
end

local function bogo_sort(arr)
  while not is_sorted(arr) do
    shuffle(arr)
  end
end

local arr = {1, 45, 756, 4569, 56, 3, 8, 5, -10, -4}
print(("Unsorted array: {%s}"):format(table.concat(arr,", ")))

bogo_sort(arr)

print(("Sorted array: {%s}"):format(table.concat(arr,", ")))
