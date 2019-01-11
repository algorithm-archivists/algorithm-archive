local function frequency_array(str)
  -- Collect all frequency values into a dict
  local map = {}
  for c in str:gmatch"." do -- Iterate over each character in str
    map[c] = (map[c] or 0) + 1 -- Increment map[c] (default 0) by 1
  end
  
  -- We have a dict of frequencies but we want it in a sorted list
  -- Dump each key value pair into an array
  local arr = {}
  for k, v in pairs(map) do
    arr[#arr + 1] = {k, v}
  end
  table.sort(arr, function(a, b) return a[2] > b[2] end) -- Sort by frequency descending
  return arr
end

function build_huffman_tree(message)

  if #message == 0 then return end

  local freq = frequency_array(message)

  while #freq > 1 do -- Repeat until we have only 1 node
    
    -- Take two of the least frequent nodes
    local node1, node2 = table.remove(freq), table.remove(freq)
    
        -- Group node values in first index, and sum of node frequencies in second
    local node3 = { {node1[1], node2[1] }, node1[2] + node2[2] }
  
    local i = 1
    while i <= #freq and freq[i][2] <= node3[2] do -- Sorted insertion, faster than inserting then sorting again
      i = i + 1
    end
    
    table.insert(freq, i, node3)
  end
  
  return freq[1][1] -- Return value of only element in freq array
end

local function _create_codebook(node, codebook, code)
  if not node then
    return
  elseif type(node) == "string" then
    codebook[node] = code -- if node is a leaf then add it to codebook
  else
    _create_codebook(node[1], codebook, code .. "0") -- Left side
    _create_codebook(node[2], codebook, code .. "1") -- Right side
  end
end

function create_codebook(tree)
  local codebook = {}
  _create_codebook(tree, codebook, "")
  return codebook
end

function huffman_encode(codebook, message)
  local encoded_chars = {}
  for c in message:gmatch"." do -- Iterate over each character in message
    encoded_chars[#encoded_chars + 1] = codebook[c]
  end
  return table.concat(encoded_chars) -- table.concat to avoid slow string bufferin
end

local function _huffman_decode(node, bitstring, i)
  if type(node) == "string" then
    return node, i -- If it's a leaf node then return the value along with the next bit to read
  end
  if bitstring:sub(i, i) == "0" then
    return _huffman_decode(node[1], bitstring, i + 1) -- If it's 0 traverse down the left side
  elseif bitstring:sub(i, i) == "1" then
    return _huffman_decode(node[2], bitstring, i + 1) -- If it's 1 traverse down the right side
  end
end

function huffman_decode(tree, bitstring)
  -- i is the current position in the bitstring, we can track which bit we are to look at next without using string.sub
  local decoded_chars, i = {}, 1
  while i <= #bitstring do
    decoded_chars[#decoded_chars + 1], i = _huffman_decode(tree, bitstring, i)
  end
  
  return table.concat(decoded_chars)
end

local message = "bibbity_bobbity"

local tree = build_huffman_tree(message)
local codebook = create_codebook(tree)

local bitstring = huffman_encode(codebook, message)
print("Encoded: " .. bitstring)

print("Decoded: " .. huffman_decode(tree, bitstring))
