using DataStructures
using Test

# Function to check to make sure we are on the canvas
function inbounds(canvas_size, loc)

    # Make sure we are not beneath or to the left of the canvas
    if minimum(Tuple(loc)) < 1
        return false

    # Make sure we are not to the right of the canvas
    elseif loc[2] > canvas_size[2]
        return false

    # Make sure we are not above the canvas
    elseif loc[1] > canvas_size[1]
        return false
    else
        return true
    end
end

function find_neighbors(canvas, loc::CartesianIndex, old_val, new_val)
    
    # Finding north, south, east, west neighbors
    possible_neighbors = [loc + CartesianIndex(0, 1),
                          loc + CartesianIndex(1, 0),
                          loc + CartesianIndex(0, -1),
                          loc + CartesianIndex(-1, 0)]

    # Exclusing neighbors that should not be colored
    neighbors =  []
    for possible_neighbor in possible_neighbors
        if inbounds(size(canvas), possible_neighbor) &&
           canvas[possible_neighbor] == old_val
            push!(neighbors, possible_neighbor)
        end
    end

    return neighbors
end

function stack_fill!(canvas, loc::CartesianIndex, old_val, new_val)
    if new_val == old_val
        return
    end

    s = Stack{CartesianIndex}()
    push!(s, loc)

    while length(s) > 0
        current_loc = pop!(s)
        if canvas[current_loc] == old_val
            canvas[current_loc] = new_val
            possible_neighbors = find_neighbors(canvas, current_loc,
                                                old_val, new_val)
            for neighbor in possible_neighbors
                push!(s,neighbor)
            end
        end
        
    end
end


function queue_fill!(canvas, loc::CartesianIndex, old_val, new_val)
    if new_val == old_val
        return
    end

    q = Queue{CartesianIndex}()
    enqueue!(q, loc)

    # Coloring the initial location
    canvas[loc] = new_val

    while length(q) > 0
        current_loc = dequeue!(q)

        possible_neighbors = find_neighbors(canvas, current_loc,
                                            old_val, new_val)

        # Coloring as we are enqueuing neighbors
        for neighbor in possible_neighbors
            canvas[neighbor] = new_val
            enqueue!(q,neighbor)
        end
        
    end
end

function recursive_fill!(canvas, loc::CartesianIndex, old_val, new_val)

    if (old_val == new_val)
        return
    end

    canvas[loc] = new_val

    possible_neighbors = find_neighbors(canvas, loc, old_val, new_val)
    for possible_neighbor in possible_neighbors
        recursive_fill!(canvas, possible_neighbor, old_val, new_val)
    end
end

function main()

    # Creation of a 5x5 grid with a single row of 1.0 elements 
    grid = zeros(5,5)
    grid[3,:] .= 1

    # Create solution grid
    answer_grid = zeros(5,5)
    answer_grid[1:3, :] .= 1

    # Start filling at 1,1
    start_loc = CartesianIndex(1,1)

    @testset "Fill Methods" begin
        # Use recursive method and reinitialize grid
        recursive_fill!(grid, start_loc, 0.0, 1.0)
        @test grid == answer_grid

        grid[1:2,:] .= 0

        # Use queue method and reinitialize grid
        queue_fill!(grid, start_loc, 0.0, 1.0)
        @test grid == answer_grid

        grid[1:2,:] .= 0

        # Use stack method and reinitialize grid
        stack_fill!(grid, start_loc, 0.0, 1.0)
        @test grid == answer_grid
    end

end

main()
