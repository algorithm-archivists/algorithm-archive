function thomas(a::Vector{Float64}, b::Vector{Float64}, c::Vector{Float64},
                d::Vector{Float64}, n::Int64)

    soln = Vector{Float64}(undef, n)

    # Setting initial elements
    c[1] = c[1] / b[1]
    d[1] = d[1] / b[1]

    for i = 2:n
        # Scale factor is for c and d
        scale = 1.0 / (b[i] - c[i-1]*a[i])
        c[i] = c[i] * scale
        d[i] = (d[i] - a[i] * d[i-1]) * scale
    end

    # Set the last solution for back-substitution
    soln[n] = d[n]

    # Back-substitution
    for i = n-1:-1:1
        soln[i] = d[i] - c[i] * soln[i+1]
    end

    return soln

end

function main()
    a = [0.0, 2.0, 3.0]
    b = [1.0, 3.0, 6.0]
    c = [4.0, 5.0, 0.0]
    d = [7.0, 5.0, 3.0]

    println("The system,")
    println(a)
    println(b)
    println(c)
    println(d)
    println("Has the solution:")

    soln = thomas(a, b, c, d, 3)

    println(soln)
end

main()
