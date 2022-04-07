using DelimitedFiles, LinearAlgebra
using Test

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

    return [sigma * r * cos(theta) + mu[1],
            sigma * r * sin(theta) + mu[2]]

end

# This function reads in a pair of input points and performs the Cartesian
# Box--Muller transform
function polar_box_muller(input_pts, sigma, mu)
    r_0 = input_pts[1]^2 + input_pts[2]^2

    # this method is only valid for points within the unit circle
    if r_0 == 0 || r_0 > 1
        return [NaN, NaN]
    end

    return [sigma * input_pts[1] * sqrt(-2 * log(r_0) / r_0) + mu[1],
            sigma * input_pts[2] * sqrt(-2 * log(r_0) / r_0) + mu[2]]

end

function is_gaussian(input_pts; bounds = [-1 1; -1 1], dx = 0.1,
                     sigma = 1, mu = [0,0], threshold = 0.1)
    histogram = zeros(ceil(Int,(bounds[1,2]-bounds[1,1])/dx),
                      ceil(Int,(bounds[2,2]-bounds[2,1])/dx))

    for i = 1:size(input_pts)[1]
        input_x = input_pts[i, 1]
        input_y = input_pts[i, 2]
        if !(isnan(input_x) || isnan(input_y))

            bin = CartesianIndex(ceil(Int, (input_x - bounds[1,1]) / dx),
                                 ceil(Int, (input_y - bounds[2,1]) / dx))

            if bin[1] <= size(histogram)[1] && bin[1] > 0 &&
               bin[2] <= size(histogram)[2] && bin[2] > 0
                histogram[bin] += 1
            end
        end
    end

    n = sum(histogram)
    normalize!(histogram)

    rms = 0
    for i = 1:size(histogram)[1]
        x = bounds[1,1] + i*dx
        for j = 1:size(histogram)[2]
            y = bounds[2,1] + j*dx
            gaussian_value = exp(-(((x+mu[1])^2)/(2*sigma^2) +
                                   ((y+mu[2])^2)/(2*sigma^2)))
            rms += (gaussian_value - histogram[i,j])^2
        end
    end

    return sqrt(rms/n) < threshold
end

function main(n)

    # This casts the input onto the nearest square for the cartesian grids
    n = Int(ceil(sqrt(n))^2)

    cartesian_grid = create_grid(n, [0,1])
    polar_grid = create_grid(n, [-1,1])
    cartesian_rand = create_rand_dist(n, [0,1])
    polar_rand = create_rand_dist(n, [-1,1])

    cartesian_grid_output = similar(cartesian_grid)
    polar_grid_output = similar(polar_grid)
    cartesian_rand_output = similar(cartesian_rand)
    polar_rand_output = similar(polar_rand)

    # going through each pair of points and using the x,y coordinates in
    # their respective functions
    for i = 1:size(cartesian_grid)[1]
        cartesian_grid_output[i,:] .= 
            cartesian_box_muller(cartesian_grid[i,:], 1, [0,0])

        polar_grid_output[i,:] .= polar_box_muller(polar_grid[i,:], 1, [0,0])

        cartesian_rand_output[i,:] .=
            cartesian_box_muller(cartesian_rand[i,:], 1, [0,0])

        polar_rand_output[i,:] .= polar_box_muller(polar_rand[i,:], 1, [0,0])
    end

    @testset "histogram tests of Box--Muller Gaussianness" begin
        @test is_gaussian(cartesian_grid_output;
                          bounds = [-3 3; -3 3], dx = 0.3,
                          sigma = 1, mu = [0,0])
        @test is_gaussian(cartesian_rand_output;
                          bounds = [-3 3; -3 3], dx = 0.3,
                          sigma = 1, mu = [0,0])
        @test is_gaussian(polar_grid_output;
                          bounds = [-3 3; -3 3], dx = 0.3,
                          sigma = 1, mu = [0,0])
        @test is_gaussian(polar_rand_output;
                          bounds = [-3 3; -3 3], dx = 0.3,
                          sigma = 1, mu = [0,0])
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
