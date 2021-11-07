function solve_euler(timestep::Float64, n::Int64)
    euler_result = Vector{Float64}(undef, n)

    # Setting the initial condition
    euler_result[1] = 1;
    for i = 2:length(euler_result)
        euler_result[i] = euler_result[i-1] - 3.0*euler_result[i-1]*timestep
    end
    return euler_result
end

function check_result(euler_result::Vector{Float64}, threshold::Float64,
                      timestep::Float64)
    is_approx = true

    for i = 1:length(euler_result)
        time = (i - 1)*timestep
        solution = exp(-3*time);
        if (abs(euler_result[i] - solution) > threshold)
            println(euler_result[i], solution)
            is_approx = false
        end
    end

    return is_approx
end

function main()
    timestep = 0.01
    n = 100
    threshold = 0.01

    euler_result = solve_euler(timestep,n)
    is_approx = check_result(euler_result, threshold, timestep)

    println(is_approx)
end

main()
