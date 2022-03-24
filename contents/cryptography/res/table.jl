function print_table(a::Array{T, 2},
                     header::Vector{String}) where T <: Union{Char, String}
    print(" | ")
    for i = 1:length(header)
        print(string(header[i]), " | ")
    end
    println()

    print(" | ")
    for i = 1:length(header)
        print("---", " | ")
    end
    println()

    for i = 1:size(a)[1]
        print(" | ")
        for j = 1:size(a)[2]
            print(string(a[i,j]), " | ")
        end
        println()
    end
end

alphabet = [char for char = 'a':'z']
offsets = Int.([0, 0, 2, 14, 18, 21, 24])
alphabet_array = Array{Char}(undef, 26, length(offsets))

for i = 1:length(offsets)
    alphabet_array[:,i] = vcat(alphabet[offsets[i]+1:26],alphabet[1:offsets[i]])
end

header = vcat(string.(offsets))
print_table(alphabet_array, header)
