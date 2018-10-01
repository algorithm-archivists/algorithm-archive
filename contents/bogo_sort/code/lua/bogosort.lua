
local function shuffle(arr)
	for i = 1, #arr do
		local rand = math.random(#arr)
		arr[i], arr[rand] = arr[rand], arr[i]
	end
end

local function issorted(arr)
	for i = 1,#arr-1 do
		if arr[i] > arr[i+1] then
			return false
		end
	end
	return true
end

function bogosort(arr)
	while not issorted(arr) do
		shuffle(arr)
	end
end