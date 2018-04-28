struct Point
    x::Float64
    y::Float64
end

function dist(point1::Point, point2::Point)
    return sqrt((point1.x - point2.x)^2 + (point1.y - point2.y)^2)
end

function jarvis_angle(point1::Point, point2::Point, point3::Point)
#=
    # Find distances between all points
    a = dist(point3, point2)
    b = dist(point3, point1)
    c = dist(point1, point2)

    println(point1)
    println(point2)
    println(point3)
    println(a)
    println(b)
    println(c)
    println(-(c*c - a*a - b*b)/(2*a*b))
    println()
    ret_angle = acos((b*b - a*a - c*c)/(2*a*c))
    println(ret_angle)
    println()

    if(sign(point1.x - point2.x) != sign(point1.x - point3.x))
        ret_angle += 0.5*pi
    end

    if (isnan(ret_angle))
        exit(1)
    end

    return ret_angle
=#
    return (point1.x - point2.x)*(point3.x-point2.x) + 
           (point1.y - point2.y)*(point3.x-point2.y) 
end

function jarvis_march(points::Vector{Point})
    hull = Vector{Point}()

    # sorting array based on leftmost point
    sort!(points, by = item -> item.x)
    push!(hull, points[1])

    i = 1
    curr_point = points[2]

    threshold = 1e-5

    # Find angle between points
    curr_product = 0.0
    #while (curr_point != hull[1])
    while (length(hull) < 4)
        println(hull)
        for point in points
            product = 0.0
            if (i == 1)
                if (hull[i] != point)
                    product = hull[i].x*(point.x - hull[i].x)+
                              hull[i].y*(point.y - hull[i].y)
                              
                    println(product)
                end 
            else
                if (hull[i] != point && hull[i-1] != point)
                    product = (hull[i].x - hull[i-1].x)*(point.x - hull[i].x)+
                              (hull[i].y - hull[i-1].y)*(point.y - hull[i].y)
                end
            end
    
            println(hull[i])
            println(point)
            println(product)
            println(curr_product)
            println()
            if (product < curr_product || curr_product == 0)
                println(product, '\t', curr_product)
                curr_point = point
                curr_product = product
            end
        end
        push!(hull, curr_point)
        i += 1
        curr_product = 0
    end


    return hull
end

function main()
    points = [Point(2,1.9), Point(1, 1), Point(2, 4), Point(3, 1)]
    hull = jarvis_march(points)
    println(hull)
end

main()
