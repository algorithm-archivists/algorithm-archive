
local function shuffle(arr)
	local size = #tbl
	for i = size, 1, -1 do
		local rand = math.random(size)
		tbl[i], tbl[rand] = tbl[rand], tbl[i]
	end
	return tbl
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