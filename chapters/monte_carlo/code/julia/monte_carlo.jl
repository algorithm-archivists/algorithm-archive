# function to determine whether an x, y point is in the unit circle
function in_circle(x_pos::Float64, y_pos::Float64, radius::Float64)
    if (x_pos^2 + y_pos^2 < radius^2)
        return true
    else
        return false
    end
end

# function to integrate a unit circle to find pi via monte_carlo
function monte_carlo(n::Int64, radius::Float64)

    pi_count = 0
    for i = 1:n
        point_x = rand()
        point_y = rand()

        if (in_circle(point_x, point_y, radius))
            pi_count += 1
        end
    end

    pi_estimate = 4*pi_count/(n*radius^2)
    println("Percent error is: ", signif(100*(pi - pi_estimate)/pi, 3), " %")
end

monte_carlo(10000000, 0.5)
