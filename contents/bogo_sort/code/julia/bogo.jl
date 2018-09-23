using Random

function is_sorted(a::Vector{Float64})
    for i = 1:length(a)-1
        if (a[i] > a[i + 1])
            return false
        end
    end
    return true
end

function bogo_sort(a::Vector{Float64})
    while(!is_sorted(a))
        shuffle!(a)
    end
end

function main()
    a = [1, 3, 2, 4]
    bogo_sort(a)
    println(a)
end

main()
