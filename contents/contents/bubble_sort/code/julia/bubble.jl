function bubble_sort!(a::Vector{Float64})
    n = length(a)
    for i = 1:n
        for j = 1:n-1
            if(a[j] < a[j+1])
                a[j], a[j+1] = a[j+1], a[j]
            end
        end
    end
end


function main()
    a = [1., 3, 2, 4, 5, 10, 50, 7, 1.5, 0.3]
    bubble_sort!(a)
    println(a)
end

main()
