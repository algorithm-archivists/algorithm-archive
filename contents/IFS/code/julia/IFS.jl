using DelimitedFiles

# This is a function to simulate a "chaos game"
function chaos_game(n::Int, shape_points)

    # Initializing the output array and the initial point
    output_points = zeros(n,2)
    point = [rand(), rand()]

    for i = 1:n
        output_points[i,:] .= point
        point = 0.5*(rand(shape_points) .+ point)
    end

    return output_points

end

# This will generate a Sierpinski triangle with a chaos game of n points for an 
# initial triangle with three points on the vertices of an equilateral triangle:
#     A = (0.0, 0.0)
#     B = (0.5, sqrt(0.75))
#     C = (1.0, 0.0)
# It will output the file sierpinski.dat, which can be plotted after
shape_points = [[0.0, 0.0],
                [0.5, sqrt(0.75)],
                [1.0, 0.0]]
output_points = chaos_game(10000, shape_points)
writedlm("sierpinski.dat", output_points)
