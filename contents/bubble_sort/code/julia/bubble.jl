using Base: copymutable

function bubble_sort!(a::AbstractVector)
    ax     = axes(a, 1)
    n      = length(ax)
    lo, hi = first(ax), last(ax)

    for _ in lo:hi
        @inbounds for j in lo:hi-1
            if a[j] < a[j+1]
                a[j], a[j+1] = a[j+1], a[j]
            end
        end
    end

    return a
end

bubble_sort(a) =  bubble_sort!(copymutable(a))

let a = rand(1:100, 9)
    bubble_sort!(a)
    display(a)
end
