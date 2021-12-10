function create_grid(n, endpoints)

    grid_extents = endpoints[2] - endpoints[1]

    # number of points along any given axis
    # For 2D, we take the sqrt(n) and then round up
    axis_num = ceil(Int, sqrt(n))

    # we are now rounding n up to the nearest square if it was not already one
    if sqrt(n) != axis_num
       n = axis_num^2
    end 

    # Distance between each point
    dx = grid_extents / (axis_num)

    # This is warning in the case that we do not have a square number
    if sqrt(n) != axis_num
        println("Cannot evenly divide ", n, " into 2 dimensions!")
    end 

    # Initializing the array, particles along the column, dimensions along rows
    a = zeros(n, 2)

    # This works by firxt generating an N dimensional tuple with the number
    # of particles to be places along each dimension ((10,10) for 2D and n=100)
    # Then we generate the list of all CartesianIndices and cast that onto a
    # grid by multiplying by dx and subtracting grid_extents/2
    for i = 1:axis_num
        for j = 1:axis_num
            a[(i - 1) * axis_num + j, :] .=
                [i * dx + endpoints[1],
                 j * dx + endpoints[1]]
        end
    end

    return a
end

function create_rand_dist(n, endpoints)
    grid_extents = endpoints[2] - endpoints[1]
    return(rand(n,2) * grid_extents .+ endpoints[1]) 
end

# This function reads in a pair of input points and performs the Cartesian
# Box--Muller transform
function cartesian_box_muller(input_pts, sigma, mu)
    r = sqrt(-2 * log(input_pts[1]))
    theta = 2 * pi * input_pts[2]

    return [sigma * r * cos(theta) + mu,
            sigma * r * sin(theta) + mu]

end

# This function reads in a pair of input points and performs the Cartesian
# Box--Muller transform
function polar_box_muller(input_pts, sigma, mu)
    r_0 = input_pts[1]^2 + input_pts[2]^2

    # this method is only valid for points within the unit circle
    if r_0 == 0 || r_0 > 1
        return [NaN, NaN]
    end

    return [sigma * input_pts[1] * sqrt(-2 * log(r_0) / r_0) + mu,
            sigma * input_pts[2] * sqrt(-2 * log(r_0) / r_0) + mu]

end
