type Point
    x::Float64
    y::Float64
end

function dist(point1::Point, point2::Point)
    return sqrt((point1.x - point2.x)^2 + (point1.y - point2.y)^2)
end

function graham_angle(point1::Point, point2::Point, point3::Point)
    # Find distances between all points
    a = dist(point3, point2)
    b = dist(point3, point1)
    c = dist(point1, point2)

    ret_angle = acos((b*b - a*a - c*c)/(2*a*c))

    if(sign(point1.x - point2.x) != sign(point1.x - point3.x))
        ret_angle += 0.5*pi
    end

    if (isnan(ret_angle))
        exit(1)
    end

    return ret_angle

end

function ccw(a::Point, b::Point, c::Point)
    return ((b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x))
end

function graham_scan(points::Vector{Point})
    N = length(points)

    # Place the lowest point at the start of the array
    sort!(points, by = item -> item.y)
    
    # Sort all other points according to angle with that point
    other_points = sort(points[2:end], by = item -> graham_angle(Point(points[1].x - 1,points[1].y), points[1], item))

    # Place points sorted by angle back into points vector
    for i in 1:length(other_points)
        points[i+1] = other_points[i]
    end

    # M will be the point on the hull
    M = 2
    i = 3
    for i = 1:N
        while (ccw(points[M-1], points[M], points[i]) <= 0)
            if (M > 2)
                M -= 1
            # All points are collinear
            elseif (i == N)
                break
            else
                i += 1
            end
        end

        # ccw point found, updating hull and swapping points
        M += 1
        points[i], points[M] = points[M], points[i]
    end

    return points[1:M]
end

function main()
    points = [Point(2,1.9), Point(1, 1), Point(2, 4), Point(3, 1), Point(2, 0)]
    hull = graham_scan(points)
    println(hull)
end

main()

