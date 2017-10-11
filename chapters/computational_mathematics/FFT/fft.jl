#simple DFT function
function DFT(x)
    N = length(x)

    # We want two vectors here for real space (n) and frequency space (k)
    n = 0:N-1
    k = n'
    transform_matrix = exp.(-2im * pi *n *k / N)
    return transform_matrix*x

end

# Implementing the Cooley-Tukey Algorithm
function cooley_tukey(x)
    N = length(x)

    #println(N)

    if(N%2 !=0)
        println("Must be a power of 2!")
        exit(0)
    end
    if(N <= 2)
        #println("DFT_slow")
        return DFT(x)
    else
        x_odd = cooley_tukey(x[1:2:N])
        x_even = cooley_tukey(x[2:2:N])
        n = 0:N-1
        #n = n'
        half = div(N,2)
        factor = exp.(-2im*pi*n/N)
        #println(factor)
        return vcat(x_odd + factor[1:half] .* x_even,
                    x_odd + factor[half+1:N] .* x_even) 
    end

end

function approx(x, y)
    val = true
    for i = 1:length(x)
        if (abs(x[i]) - abs(y[i]) > 1e-5)
            val = false
        end
    end
    println(val)
end

function main()
    x = [0 1 0 1 0 1 0 1]
    y = cooley_tukey(x)
    for i in y
        println(i)
    end
end

main()
