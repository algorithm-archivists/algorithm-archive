using FFTW
using LinearAlgebra
using DelimitedFiles

# using the convolutional theorem
function convolve_fft(signal1::Array{T}, signal2::Array{T}) where {T <: Number}
    return ifft(fft(signal1).*fft(signal2))
end

function main()

    # sawtooth functions for x and y
    x = [float(i)/200 for i = 1:200]
    y = [float(i)/200 for i = 1:200]

    # Normalization is not strictly necessary, but good practice
    normalize!(x)
    normalize!(y)

    # cyclic convolution via the convolutional theorem
    fft_output = convolve_fft(x, y)

    # outputting convolutions to different files for plotting in external code
    # note: we are outputting just the real component because the imaginary
    #       component is virtually 0
    writedlm("fft.dat", real(fft_output))

end

main()
