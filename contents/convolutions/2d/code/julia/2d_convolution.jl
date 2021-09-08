using DelimitedFiles
using LinearAlgebra

function convolve_linear(signal::Array{T, 2}, filter::Array{T, 2},
                         output_size) where {T <: Number}

    # convolutional output
    out = Array{Float64,2}(undef, output_size)
    sum = 0

    for i = 1:output_size[1]
        for j = 1:output_size[2]
            for k = max(1, i-size(filter)[1]):i
                for l = max(1, j-size(filter)[2]):j
                    if k <= size(signal)[1] && i-k+1 <= size(filter)[1] &&
                       l <= size(signal)[2] && j-l+1 <= size(filter)[2]
                        sum += signal[k,l] * filter[i-k+1, j-l+1]
                    end
                end
            end

            out[i,j] = sum
            sum = 0
        end
    end

    return out
end

function create_gaussian_kernel(kernel_size)

    kernel = zeros(kernel_size, kernel_size)

    # The center must be offset by 0.5 to find the correct index
    center = kernel_size * 0.5 + 0.5

    sigma = sqrt(0.1*kernel_size)

    for i = 1:kernel_size
        for j = 1:kernel_size
            kernel[i,j] = exp(-((i-center)^2 + (j-center)^2) / (2*sigma^2))
        end
    end

    return normalize(kernel)
    
end

function create_sobel_operators()
    Sx = [1.0, 2.0, 1.0]*[-1.0 0.0 1.0] / 9
    Sy = [-1.0, 0.0, 1.0]*[1.0 2.0 1.0] / 9

    return Sx, Sy
end

function compute_sobel(signal)
    Sx, Sy = create_sobel_operators()

    Gx = convolve_linear(signal, Sx, size(signal) .+ size(Sx))
    Gy = convolve_linear(signal, Sy, size(signal) .+ size(Sy))

    return sqrt.(Gx.^2 .+ Gy.^2)
end

# Simple function to create a square grid with a circle embedded inside of it
function create_circle(image_resolution, grid_extents, radius)
    out = zeros(image_resolution, image_resolution)

    for i = 1:image_resolution
        x_position = ((i-1)*grid_extents/image_resolution)-0.5*grid_extents
        for j = 1:image_resolution
            y_position = ((j-1)*grid_extents/image_resolution)-0.5*grid_extents
            if x_position^2 + y_position^2 <= radius^2
                out[i,j] = 1.0
            end
        end
    end 

    return out
end

function main()

    # Random distribution in x
    x = rand(100, 100)

    # Gaussian signals
    y = [exp(-(((i-50)/100)^2 + ((j-50)/100)^2)/.01) for i = 1:100, j=1:100]

    # Normalization is not strictly necessary, but good practice
    normalize!(x)
    normalize!(y)

    # full convolution, output will be the size of x + y
    full_linear_output = convolve_linear(x, y, size(x) .+ size(y))

    # simple boundaries
    simple_linear_output = convolve_linear(x, y, size(x))

    # outputting convolutions to different files for plotting in external code
    writedlm("full_linear.dat", full_linear_output)
    writedlm("simple_linear.dat", simple_linear_output)

    # creating simple circle and 2 different Gaussian kernels
    circle = create_circle(50,2,0.5)

    normalize!(circle)

    small_kernel = create_gaussian_kernel(3)
    large_kernel = create_gaussian_kernel(25)

    small_kernel_output = convolve_linear(circle, small_kernel,
                                          size(circle).+size(small_kernel))
    large_kernel_output = convolve_linear(circle, large_kernel,
                                          size(circle).+size(large_kernel))

    writedlm("small_kernel.dat", small_kernel_output)
    writedlm("large_kernel.dat", large_kernel_output)

    # Using the circle for Sobel operations as well
    sobel_output = compute_sobel(circle)

    writedlm("sobel_output.dat", sobel_output)

end
