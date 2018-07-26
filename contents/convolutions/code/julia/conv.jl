function conv(signal1::Vector{Complex}, signal2::Vector{Complex})
    n = length(signal1) + length(signal2)
    out = Vector{Complex}(n)
    sum = 0

    for i = 0:n
        for j = 0:i
            if(j < length(signal1))
                sum += signal1[j] * signal2[i-j]
            end
        end
        out[i] = sum
        sum = 0
    end

    return out
end

function conv_fft(signal1::Vector{Complex}, signal2::Vector{Complex})
    return ifft(fft(signal1).*fft(signal2))
end

