using KernelAbstractions
using CUDA

if has_cuda_gpu()
    using CUDAKernels
end

function create_grid(n, endpoints; AT = Array)

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
    a = AT(zeros(n, 2))

    # This works by firxt generating an N dimensional tuple with the number
    # of particles to be places along each dimension ((10,10) for 2D and n=100)
    # Then we generate the list of all CartesianIndices and cast that onto a
    # grid by multiplying by dx and subtracting grid_extents/2
    for i = 1:axis_num
        for j = 1:axis_num
            a[(i - 1) * axis_num + j, 1] = i * dx + endpoints[1]
            a[(i - 1) * axis_num + j, 2] = j * dx + endpoints[1]
        end
    end

    return a
end

function create_rand_dist(n, endpoints; AT = Array)
    grid_extents = endpoints[2] - endpoints[1]
    return(AT(rand(n,2)) * grid_extents .+ endpoints[1]) 
end

# This function reads in a pair of input points and performs the Cartesian
# Box--Muller transform
@kernel function polar_muller_noreplacement!(input_pts, output_pts, sigma, mu)
    tid = @index(Global, Linear)
    @inbounds r_0 = input_pts[tid, 1]^2 + input_pts[tid, 2]^2

    # this method is only valid for points within the unit circle
    if r_0 == 0 || r_0 > 1
        @inbounds output_pts[tid,1] = NaN
        @inbounds output_pts[tid,2] = NaN
    else
        @inbounds output_pts[tid,1] = sigma * input_pts[tid,1] *
                                      sqrt(-2 * log(r_0) / r_0) + mu
        @inbounds output_pts[tid,2] = sigma * input_pts[tid, 2] *
                                      sqrt(-2 * log(r_0) / r_0) + mu
    end

end

@kernel function polar_muller_replacement!(input_pts, output_pts, sigma, mu)
    tid = @index(Global, Linear)
    @inbounds r_0 = input_pts[tid, 1]^2 + input_pts[tid, 2]^2

    while r_0 > 1 || r_0 == 0
        p1 = rand()*2-1
        p2 = rand()*2-1
        r_0 = p1^2 + p2^2
    end

    @inbounds output_pts[tid,1] = sigma * input_pts[tid,1] *
                                  sqrt(-2 * log(r_0) / r_0) + mu
    @inbounds output_pts[tid,2] = sigma * input_pts[tid, 2] *
                                  sqrt(-2 * log(r_0) / r_0) + mu
end


function polar_box_muller!(input_pts, output_pts, sigma, mu;
                           numthreads = 256, numcores = 4,
                           f = polar_muller_noreplacement!)
    if isa(input_pts, Array)
        kernel! = f(CPU(), numcores)
    else
        kernel! = f(CUDADevice(), numthreads)
    end
    kernel!(input_pts, output_pts, sigma, mu, ndrange=size(input_pts)[1])
end


@kernel function cartesian_kernel!(input_pts, output_pts, sigma, mu)
    tid = @index(Global, Linear)

    @inbounds r = sqrt(-2 * log(input_pts[tid,1]))
    @inbounds theta = 2 * pi * input_pts[tid, 2]

    @inbounds output_pts[tid,1] = sigma * r * cos(theta) + mu
    @inbounds output_pts[tid,2] = sigma * r * sin(theta) + mu
end

function cartesian_box_muller!(input_pts, output_pts, sigma, mu;
                               numthreads = 256, numcores = 4)
    if isa(input_pts, Array)
        kernel! = cartesian_kernel!(CPU(), numcores)
    else
        kernel! = cartesian_kernel!(CUDADevice(), numthreads)
    end

    kernel!(input_pts, output_pts, sigma, mu, ndrange=size(input_pts)[1])
end

function main()

    input_pts = create_rand_dist(4096^2,[0,1])
    output_pts = create_rand_dist(4096^2,[0,1])

    wait(cartesian_box_muller!(input_pts, output_pts, 1, 0))
    @time wait(cartesian_box_muller!(input_pts, output_pts, 1, 0))
    wait(polar_box_muller!(input_pts, output_pts, 1, 0))
    @time wait(polar_box_muller!(input_pts, output_pts, 1, 0))

    if has_cuda_gpu()
        input_pts = create_rand_dist(4096^2,[0,1], AT = CuArray)
        output_pts = create_rand_dist(4096^2,[0,1], AT = CuArray)

        wait(cartesian_box_muller!(input_pts, output_pts, 1, 0))
        CUDA.@time wait(cartesian_box_muller!(input_pts, output_pts, 1, 0))
        wait(polar_box_muller!(input_pts, output_pts, 1, 0))
        CUDA.@time wait(polar_box_muller!(input_pts, output_pts, 1, 0))
    end

end

main()
