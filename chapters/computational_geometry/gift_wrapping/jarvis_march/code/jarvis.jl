# Struct to hold an x, y position. A tuple would also be fine.
struct Pos
    x::Float64
    y::Float64
end

# This returns a measure of how far "left" the vector is with a cross product
function cross(point1::Pos, point2::Pos, point3::Pos)
    vec1 = Pos(point2.x - point1.x, point2.y - point1.y)
    vec2 = Pos(point3.x - point2.x, point3.y - point2.y)
    ret_angle = vec1.x*vec2.y - vec1.y*vec2.x
    return ret_angle*ret_angle
end

function jarvis_march(points::Vector{Pos})
    hull = Vector{Pos}()

    # sorting array based on leftmost point
    sort!(points, by = item -> item.x)
    push!(hull, points[1])

    i = 1
    curr_point = points[2]

    # Find angle between points
    curr_product = cross(Pos(0,0), hull[1], curr_point)

    # We will hold a temp variable with the highest cross product as we iterate
    # through all the points and move our hull vector forward.
    while (curr_point != hull[1])
        for point in points
            product = 0.0

            # Special case for the first element when there is no hull[i-1]
            if (i == 1)
                if (hull[i] != point)
                    product = cross(Pos(0,0), hull[i], point)
                end
            else
                if (hull[i] != point && hull[i-1] != point)
                    product = cross(hull[i-1], hull[i], point)
                end
            end
            if (product > curr_product)
                curr_point = point
                curr_product = product
            end
        end

        # Pushing to hull, moving simulation forward and resetting the product
        push!(hull, curr_point)
        curr_product = 0
        i += 1
    end

    return hull
end

function main()

    # These points are chosen such that there is a clearly defined hull with
    # several interior points. As a note, these will be generated either
    # randomly or via some mesh in practice.
    points = [Pos(2,1.5), Pos(1, 1), Pos(2, 4), Pos(3, 1), Pos(2,2), Pos(2,0.5)]
    hull = jarvis_march(points)
    println(hull)
end

main()
