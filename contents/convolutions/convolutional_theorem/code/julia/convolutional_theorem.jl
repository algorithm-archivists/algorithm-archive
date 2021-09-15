using FFTW
using DelimitedFiles

# using the convolutional theorem
function convolve_fft(signal1::Array{T}, signal2::Array{T}) where {T <: Number}
    return ifft(fft(signal1).*fft(signal2))
end

function main()

    # Random distribution in x
    x = rand(100)

    # Gaussian signals
    y = [exp(-((i-50)/100)^2/.01) for i = 1:100]

    # Normalization is not strictly necessary, but good practice
    normalize!(x)
    normalize!(y)

    # cyclic convolution via the convolutional theorem
    fft_output = convolve_fft(x, y)

    # outputting convolutions to different files for plotting in external code
    writedlm("fft.dat", fft_output)

end
