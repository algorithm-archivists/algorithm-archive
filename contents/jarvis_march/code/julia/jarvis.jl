struct Pos
    x::Float64
    y::Float64
end

function jarvis_cross(point1::Pos, point2::Pos, point3::Pos)
    vec1 = Pos(point2.x - point1.x, point2.y - point1.y)
    vec2 = Pos(point3.x - point2.x, point3.y - point2.y)
    ret_cross = vec1.x*vec2.y - vec1.y*vec2.x
    return ret_cross*ret_cross
end

function jarvis_march(points::Vector{Pos})
    hull = Vector{Pos}()

    # sorting array based on leftmost point
    sort!(points, by = item -> item.x)
    push!(hull, points[1])

    i = 1
    curr_point = points[2]

    # Find cross product between points
    curr_product = jarvis_cross(Pos(0,0), hull[1], curr_point)
    while (curr_point != hull[1])
        for point in points
                product = 0.0
            if (i == 1)
                if (hull[i] != point)
                    product = jarvis_cross(Pos(0,0), hull[i], point)
                end
            else
                if (hull[i] != point && hull[i-1] != point)
                    product = jarvis_cross(hull[i-1], hull[i], point)
                end
            end
            if (product > curr_product)
                curr_point = point
                curr_product = product
            end
        end
        push!(hull, curr_point)
        curr_product = 0
        i += 1
    end

    return hull
end

function main()

    points = [Pos(2,1.5), Pos(1, 1), Pos(2, 4), Pos(3, 1)]
    hull = jarvis_march(points)
    println(hull)
end

main()
