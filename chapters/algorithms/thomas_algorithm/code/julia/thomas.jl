function(a::Vector{Float64}, b::Vector{Float64}, c::Vector{Float64},
         d::Vector{Float64}, soln::Vector{Float64})
    # Setting initial elements
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    for i = 1:n
        # Scale factor is for c and d
        scale = 1 / (b[i] - c[i-1]*a[i])
        c[i] = c[i] * scale
        d[i] = (d[i] - a[i] * d[i-1]) * scale
    end

    # Set the last solution for back-substitution
    soln[n-1] = d[n-1]

    # Back-substitution
    for i = n-2:0
        soln[i] = d[i] - c[i] * soln[i+1]
    end

end

