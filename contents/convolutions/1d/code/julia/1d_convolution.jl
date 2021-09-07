using DelimitedFiles
using LinearAlgebra

function convolve_cyclic(signal::Array{T, 1},
                         filter::Array{T, 1}) where {T <: Number}

    # output size will be the size of sign
    output_size = max(length(signal), length(filter))

    # convolutional output
    out = Array{Float64,1}(undef,output_size)
    sum = 0

    for i = 1:output_size
        for j = 1:output_size
            sum += get(signal, mod1(j, output_size), 0) * get(filter, mod1(i-j, output_size), 0)
        end

        out[i] = sum
        sum = 0

    end

    return out
end

function convolve_linear(signal::Array{T, 1}, filter::Array{T, 1},
                         output_size) where {T <: Number}

    # convolutional output
    out = Array{Float64,1}(undef, output_size)
    sum = 0

    for i = 1:output_size
        for j = max(1, i-length(filter)):i
            if j <= length(signal) && i-j+1 <= length(filter)
                sum += signal[j] * filter[i-j+1]
            end
        end

        out[i] = sum
        sum = 0
    end

    return out
end

function main()

    # sawtooth functions for x and y
    x = [float(i)/200 for i = 1:200]
    y = [float(i)/200 for i = 1:200]

    # Normalization is not strictly necessary, but good practice
    normalize!(x)
    normalize!(y)

    # full convolution, output will be the size of x + y - 1
    full_linear_output = convolve_linear(x, y, length(x) + length(y) - 1)

    # simple boundaries
    simple_linear_output = convolve_linear(x, y, length(x))

    # cyclic convolution
    cyclic_output = convolve_cyclic(x, y)

    # outputting convolutions to different files for plotting in external code
    writedlm("full_linear.dat", full_linear_output)
    writedlm("simple_linear.dat", simple_linear_output)
    writedlm("cyclic.dat", cyclic_output)

end
