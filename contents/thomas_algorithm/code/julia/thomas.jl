function thomas(a::Vector{Float64}, b::Vector{Float64}, c::Vector{Float64},
                d::Vector{Float64}, n::Int64)

    x = copy(d)
    c_prime = copy(c)

    # Setting initial elements
    c_prime[1] /= b[1]
    x[1] /= b[1]

    for i = 2:n
        # Scale factor is for c_prime and x
        scale = 1.0 / (b[i] - c_prime[i-1]*a[i])
        c_prime[i] *= scale
        x[i] = (x[i] - a[i] * x[i-1]) * scale
    end

    # Back-substitution
    for i = n-1:-1:1
        x[i] -= (c_prime[i] * x[i+1])
    end

    return x

end

function main()
    a = [0.0, 2.0, 3.0]
    b = [1.0, 3.0, 6.0]
    c = [4.0, 5.0, 0.0]
    d = [7.0, 5.0, 3.0]

    println(
        """The system
        $(join((b[1], c[1], "",   "|", d[1]), "\t"))
        $(join((a[2], b[2], c[2], "|", d[2]), "\t"))
        $(join(("",   a[3], b[3], "|", d[3]), "\t"))
        Has the solution:"""
    )

    soln = thomas(a, b, c, d, 3)

    println(soln)
end

main()
