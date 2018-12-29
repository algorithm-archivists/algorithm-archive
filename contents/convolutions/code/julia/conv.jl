function conv(signal1::AbstractVector{<:Complex}, signal2::Vector{<:Complex})
    n = length(signal1) + length(signal2) - 1

    [sum(
        signal1[j] * signal2[i-(j-1)]
        for j = 1:i
	if j in eachindex(signal1) && (i-(j-1)) in eachindex(signal2)
    ) for i = 1:n]
end

function conv_fft(signal1::Vector{<:Complex}, signal2::Vector{<:Complex})
    return ifft(fft(signal1).*fft(signal2))
end

