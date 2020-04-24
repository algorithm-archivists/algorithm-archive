# function to determine whether an x, y point is in the unit circle
function in_circle(x_pos::Float64, y_pos::Float64)

    # Setting radius to 1 for unit circle
    radius = 1
    return x_pos^2 + y_pos^2 < radius^2
end

# function to integrate a unit circle to find pi via monte_carlo
function monte_carlo(n::Int64)

    pi_count = 0
    for i = 1:n
        point_x = rand()
        point_y = rand()

        if (in_circle(point_x, point_y))
            pi_count += 1
        end
    end

    # This is using a quarter of the unit sphere in a 1x1 box.
    # The formula is pi = (box_length^2 / radius^2) * (pi_count / n), but we
    #     are only using the upper quadrant and the unit circle, so we can use
    #     4*pi_count/n instead
    pi_estimate = 4*pi_count/n
    println("The pi estimate is: ", pi_estimate)
    println("Percent error is: ", signif(100 * abs(pi_estimate - pi) / pi, 3), " %")
end

monte_carlo(10000000)
