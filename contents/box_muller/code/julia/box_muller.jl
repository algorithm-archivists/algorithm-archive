using DelimitedFiles

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

    # Initializing the array, particles along the column, dimensions along rows
    a = zeros(n, 2)

    # This loops over the relevant dimensions
    for i = 1:axis_num
        for j = 1:axis_num
            a[(i - 1) * axis_num + j, :] .=
                [(i - 0.5) * dx + endpoints[1],
                 (j - 0.5) * dx + endpoints[1]]
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

function main(n)
    cartesian_grid = create_grid(n, [0,1])
    polar_grid = create_grid(n, [-1,1])
    cartesian_rand = create_rand_dist(n, [0,1])
    polar_rand = create_rand_dist(n, [-1,1])

    cartesian_grid_output = similar(cartesian_grid)
    polar_grid_output = similar(polar_grid)
    cartesian_rand_output = similar(cartesian_rand)
    polar_rand_output = similar(polar_rand)

    for i = 1:size(cartesian_grid)[1]
        cartesian_grid_output[i,:] .= cartesian_box_muller(cartesian_grid[i,:],
                                                           1, 0)
        polar_grid_output[i,:] .= polar_box_muller(polar_grid[i,:], 1,0)
        cartesian_rand_output[i,:] .= cartesian_box_muller(cartesian_rand[i,:],
                                                           1, 0)
        polar_rand_output[i,:] .= polar_box_muller(polar_rand[i,:], 1, 0)
    end

    writedlm("cartesian_grid_output.dat", cartesian_grid_output)
    writedlm("polar_grid_output.dat", polar_grid_output)
    writedlm("cartesian_rand_output.dat", cartesian_rand_output)
    writedlm("polar_rand_output.dat", polar_rand_output)

    writedlm("cartesian_grid.dat", cartesian_grid)
    writedlm("polar_grid.dat", polar_grid)
    writedlm("cartesian_rand.dat", cartesian_rand)
    writedlm("polar_rand.dat", polar_rand)
end
