using DelimitedFiles

function create_grid(n, endpoints; dims = 2)

    grid_extents = endpoints[2] - endpoints[1]

    # number of points along any given axis
    # For 2D, we take the sqrt(n) and then round up
    axis_num = ceil(Int, n^(1/dims))

    # we are now rounding n up to the nearest square if it was not already one
    if n^(1/dims) != axis_num
       n = axis_num^dims
    end 

    # Distance between each point
    dx = grid_extents / (axis_num)

    # Initializing the array, particles along the column, dimensions along rows
    a = zeros(n, dims)

    # This works by firxt generating an N dimensional tuple with the number
    # of particles to be places along each dimension ((10,10) for 2D and n=100)
    # Then we generate the list of all CartesianIndices and cast that onto a
    # grid by multiplying by dx and subtracting grid_extents/2
    k = 1
    for i in CartesianIndices(Tuple([axis_num for j = 0:(dims-1)]))
        if k <= size(a)[1]
            a[k,:] .= (Tuple(i).-0.5).*dx.+endpoints[1]
        end
        k += 1
    end 

    return a
end

function create_rand_dist(n, endpoints; dims=2)
    grid_extents = endpoints[2] - endpoints[1]
    return(rand(n,dims) * grid_extents .+ endpoints[1]) 
end

# This function reads in a pair of input points and performs the Cartesian
# Box--Muller transform
function cartesian_box_muller(input_pts, sigma, mu)

    output = zeros(length(input_pts)) 
    components = zeros(length(input_pts)) 

    components[end] = sqrt(-2 * log(input_pts[end]))
    output[:] .= components[end]

    if length(output) > 1
        components[end-1] = 2*pi*input_pts[end-1]

        for i = 1:length(output)-2
            components[i] = pi*input_pts[i]
        end

        for i = 2:length(output)
            output[i-1] *= cos(components[i-1])
            output[i:end] .*= sin(components[i-1])
        end 

    else
        #output[1] = -1*sign(input_pts[1])*output[1]
    end 

    return sigma .* output .+ mu

end

function main(n; dims = 2, sigma = 1, mu = 0)

    if n^(1/dims) != ceil(Int, n^(1/dims))
        n = ceil(Int, n^(1/dims))^dims
    end

    cartesian_grid = zeros(n,dims)
    cartesian_rand = zeros(n,dims)
    if dims == 1
        cartesian_grid = create_grid(n, [0,1]; dims = dims)
        cartesian_rand = create_rand_dist(n, [0,1]; dims = dims)
    else
        cartesian_grid = create_grid(n, [0,1]; dims = dims)
        cartesian_rand = create_rand_dist(n, [0,1]; dims = dims)
    end

    cartesian_grid_output = similar(cartesian_grid)
    cartesian_rand_output = similar(cartesian_rand)

    println(size(cartesian_grid)[1])
    for i = 1:size(cartesian_grid)[1]
        cartesian_grid_output[i,:] .= cartesian_box_muller(cartesian_grid[i,:],
                                                           sigma, mu)
        cartesian_rand_output[i,:] .= cartesian_box_muller(cartesian_rand[i,:],
                                                           sigma, mu)
    end

    writedlm("cartesian_grid_output.dat", cartesian_grid_output)
    writedlm("cartesian_rand_output.dat", cartesian_rand_output)

    writedlm("cartesian_grid.dat", cartesian_grid)
    writedlm("cartesian_rand.dat", cartesian_rand)
end
