using Base: copymutable

function bubble_sort!(a::AbstractVector)
    n = length(a)

    for _ in 1:n
        @inbounds for j in 1:n-1
            if a[j] < a[j+1]
                a[j], a[j+1] = a[j+1], a[j]
            end
        end
    end

    return a
end

bubble_sort(a) =  bubble_sort!(copymutable(a))

let a = [1, 9, 2, 8, 3, 7, 4, 6, 5]
    bubble_sort!(a)
    display(a)
end
