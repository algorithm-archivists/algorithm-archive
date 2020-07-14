using FFTW

#simple DFT function
function DFT(x)
    N = length(x)

    # We want two vectors here for real space (n) and frequency space (k)
    n = 0:N-1
    k = n'
    transform_matrix = exp.(-2im*pi*n*k/N)
    return transform_matrix*x

end

# Implementing the Cooley-Tukey Algorithm
function cooley_tukey(x)
    N = length(x)

    if (N > 2)
        x_odd = cooley_tukey(x[1:2:N])
        x_even = cooley_tukey(x[2:2:N])
    else
        x_odd = x[1]
        x_even = x[2]
    end
    n = 0:N-1
    half = div(N,2)
    factor = exp.(-2im*pi*n/N)
    return vcat(x_odd .+ x_even .* factor[1:half],
                x_odd .- x_even .* factor[1:half])

end

function bitreverse(a::Array)
    # First, we need to find the necessary number of bits
    digits = convert(Int,ceil(log2(length(a))))

    indices = [i for i = 0:length(a)-1]

    bit_indices = []
    for i = 1:length(indices)
        push!(bit_indices, bitstring(indices[i]))
    end

    # Now stripping the unnecessary numbers
    for i = 1:length(bit_indices)
        bit_indices[i] = bit_indices[i][end-digits:end]
    end

    # Flipping the bits
    for i =1:length(bit_indices)
        bit_indices[i] = reverse(bit_indices[i])
    end

    # Replacing indices
    for i = 1:length(indices)
        indices[i] = 0
        for j = 1:digits
            indices[i] += 2^(j-1) * parse(Int, string(bit_indices[i][end-j]))
        end
       indices[i] += 1
    end

    b = [float(i) for i = 1:length(a)]
    for i = 1:length(indices)
        b[i] = a[indices[i]]
    end

    return b
end

function iterative_cooley_tukey(x)
    N = length(x)
    logN = convert(Int,ceil(log2(length(x))))
    bnum = div(N,2)
    stride = 0;

    x = bitreverse(x)

    z = [Complex(x[i]) for i = 1:length(x)]
    for i = 1:logN
       stride = div(N, bnum)
       for j = 0:bnum-1
           start_index = j*stride + 1
           y = butterfly(z[start_index:start_index + stride - 1])
           for k = 1:length(y)
               z[start_index+k-1] = y[k]
           end
       end
       bnum = div(bnum,2)
    end

    return z
end

function butterfly(x)
    N = length(x)
    half = div(N,2)
    n = [i for i = 0:N-1]
    half = div(N,2)
    factor = exp.(-2im*pi*n/N)

    y = [0 + 0.0im for i = 1:length(x)]

    for i = 1:half
        y[i] = x[i] + x[half+i]*factor[i]
        y[half+i] = x[i] - x[half+i]*factor[i]
    end

    return y
end

function main()
    x = rand(128)
    y = cooley_tukey(x)
    z = iterative_cooley_tukey(x)
    w = fft(x)
    if(isapprox(y, w))
        println("Recursive Cooley Tukey matches fft() from FFTW package.")
    end
    if(isapprox(z, w))
        println("Iterative Cooley Tukey matches fft() from FFTW package.")
    end
end

main()
