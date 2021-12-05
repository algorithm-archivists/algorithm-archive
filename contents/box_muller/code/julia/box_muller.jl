function create_grid(n, grid_extents)

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
            a[(i-1)*axis_num+j, :] .= [i*dx-(grid_extents+dx)/2,
                                       j*dx-(grid_extents+dx)/2]
        end
    end

    return a
end

function create_rand_dist(n, grid_extents)
    return(rand(n,2) * grid_extents .- grid_extents / 2)
end

function cartesian_box_muller()

end

function polar_box_muller()

end
