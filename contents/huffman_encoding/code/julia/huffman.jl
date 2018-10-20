# This is for the PriorityQueue
using DataStructures

struct Leaf
    weight::Int64
    key::Char
end

struct Branch
    right::Union{Leaf, Branch}
    left::Union{Leaf, Branch}
    weight::Int64
end

const Node = Union{Leaf, Branch}
isbranch(branch::Branch) = true
isbranch(other::T) where {T} = false

function codebook_recurse!(leaf::Leaf, code::String,
                          dict::Dict{Char,String})
    dict[leaf.key] = code
end

function codebook_recurse!(branch::Branch, code::String,
                          dict::Dict{Char,String})
    codebook_recurse!(branch.left, string(code, "1"), dict)
    codebook_recurse!(branch.right, string(code, "0"), dict)
end

# This will depth-first search through the tree
# to create bitstrings for each character.
# Note: Any depth-first search method will work
# This outputs encoding Dict to be used for encoding
function create_codebook(n::Node)
    codebook = Dict{Char,String}()
    codebook_recurse!(n, "", codebook)
    return codebook
end

# This outputs huffman tree to generate dictionary for encoding
function create_tree(phrase::String)

    # creating weights
    weights = PriorityQueue()
    for i in phrase
        temp_string = string(i)
        if (haskey(weights, temp_string))
            weights[temp_string] += 1
        else
            weights[temp_string] = 1
        end
    end

    # Creating all nodes to iterate through
    nodes = PriorityQueue{Node, Int64}()
    while(length(weights) > 0)
        weight = peek(weights)[2]
        key = dequeue!(weights)[1]
        temp_node = Leaf(weight, key)
        enqueue!(nodes, temp_node, weight)
    end

    while(length(nodes) > 1)
        node1 = dequeue!(nodes)
        node2 = dequeue!(nodes)
        temp_node = Branch(node1, node2, node1.weight + node2.weight)
        enqueue!(nodes, temp_node, temp_node.weight)
    end

    huffman_tree = dequeue!(nodes)
    return huffman_tree

end

function encode(codebook::Dict{Char, String}, phrase::String)
    final_bitstring = ""
    for i in phrase
        final_bitstring = final_bitstring * codebook[i]
    end

    return final_bitstring
end

function decode(huffman_tree::Node, bitstring::String)
    current = huffman_tree
    final_string = ""
    for i in bitstring
        if (i == '1')
            current = current.left
        else
            current = current.right
        end
        if (!isbranch(current))
            final_string = final_string * string(current.key)
            current = huffman_tree
        end
    end

    return final_string
end

function two_pass_huffman(phrase::String)
    huffman_tree = create_tree(phrase)
    codebook = create_codebook(huffman_tree)
    println(codebook)
    bitstring = encode(codebook, phrase)
    final_string = decode(huffman_tree, bitstring)
    println(bitstring)
    println(final_string)
end

two_pass_huffman("bibbity bobbity")
